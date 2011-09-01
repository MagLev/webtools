ENV["RACK_ENV"] = "test"
require 'minitest/autorun'
require 'rack/test'
require 'web_tools'
require 'mocha'

class DebuggerTest < MiniTest::Unit::TestCase
  include Rack::Test::Methods
  WebTools::Debugger

  def app
    WebTools::Debugger
  end

  class TestObject
    attr_reader :test, :test2, :my_clone
  end

  def thread_foo
    some = 'context'
    obj = TestObject.new.tap do |o|
      o.instance_eval do
        @test = 'test'
        @test2 = 'test2'
        @my_clone = self.dup
      end
    end
    raise Exception, "thread_foo" unless Thread.current[:skip_test_exception]
    Thread.current[:result] = [200,
                               {"Content-Type" => "text/plain"},
                               ["Sucess!"]]
  end

  def process
    if @process.nil?
      res = Maglev::Debugger.debug do
        thread_foo()
      end
      Thread.pass
      @process = res[:result].thread
    end
    @process
  end

  def json
    JSON.load(last_response.body)
  end

  def setup
    Maglev.expects(:abort_transaction).returns(true)
    @process.exit if @process
    @process = nil
  end

  def test_process
    get "/process"
    assert_equal 200, last_response.status
  end

  def test_process_oop
    get "/process/#{process.object_id}"
    assert_equal 200, last_response.status
  end

  def test_process_oop_frames
    get "/process/#{process.object_id}/frames"
    assert_equal 200, last_response.status
  end

  def test_process_oop_frames_idx
    get "/process/#{process.object_id}/frames/0"
    assert_equal 200, last_response.status
  end

  def test_process_oop_frames_idx_objects
    get "/process/#{process.object_id}/frames/0/objects"
    assert_equal 200, last_response.status
  end

  def test_process_oop_frames_idx_objects_obj
    get "/process/#{process.object_id}/frames/0/objects/fake"
    assert_equal 200, last_response.status
  end

  def test_process_oop_frames_idx_objects_obj_objects
    get "/process/#{process.object_id}/frames/0/objects/fake/objects"
    assert_equal 200, last_response.status
  end

  def test_process_oop_frames_idx_objects_obj_objects_obj
    get "/process/#{process.object_id}/frames/0/objects/fake/objects/fake"
    assert_equal 200, last_response.status
  end

  def test_process_list
    process
    get "/process"
    assert last_response.ok?
    assert_equal Array, json.class
    assert json.any? {|h| h["label"] =~ /GsProcess.*sleep/ }
    assert json.any? {|h| h["process_id"] == process.object_id }
  end

  def test_shows_object_log_errors
    DebuggerLogEntry.create_continuation_labeled("test continuation")
    get "/process"
    assert json.any? {|h| h["label"] =~ /test continuation/ }
  end

  def test_retrieves_errors_from_object_log
    DebuggerLogEntry.create_continuation_labeled("test continuation")
    get "/process/#{ObjectLog.errors.last.object_id}"
    assert_match /test continuation/, json["label"]
  end

  def test_retrieves_process_by_id
    get "/process/#{process.object_id}"
    assert_match /GsProcess.*sleep/, json["label"]
    assert_equal process.object_id, json["process_id"]
  end

  def test_get_process_frames
    get "/process/#{process.object_id}/frames"
    assert_equal Array, json.class
    refute_nil json.first["method_name"]
  end

  def test_get_process_frames_without_debug_info
    get "/process/#{process.object_id}/frames"
    assert_nil json.first["debug_info"]
  end

  def test_get_top_frame
    get "/process/#{process.object_id}/frames/0"
    refute_nil json["method_name"]
    refute_nil json["source_location"]
  end

  def test_top_frame_with_debug_info
    get "/process/#{process.object_id}/frames/0"
    refute_nil json["debug_info"]
    # assert_match /#{self.class.name}/, json["debug_info"]["self"]
    assert json["debug_info"].has_key?("context")
    # assert_match /#{self.class.name}/, json["debug_info"]["(__self__)"]
  end

  def test_has_returned_the_process_to_the_raising_frame
    get "/process/#{process.object_id}/frames/0"
    assert_equal "raise", json["method_name"]
    assert_match /Kernel.rb/, json["source_location"]
    refute_nil json["debug_info"]
  end

  def test_can_inspect_the_frame_context
    get "/process/#{process.object_id}/frames/1/objects"
    assert_equal "context", json["some"]
    assert_match /TestObject/, json["obj"]
  end

  def test_show_object
    get "/process/#{process.object_id}/frames/1/objects/obj"
    assert_equal "test", json["@test"]
    assert_equal "test2", json["@test2"]
    assert_match /TestObject/, json["@my_clone"]
  end

  def test_returns_a_1_level_hash_with_the_instance_variables_and_the_SELF_key
    get "/process/#{process.object_id}/frames/1/objects/obj"
    assert_match /TestObject/, json["(__self__)"]
  end

  def test_inspect_into_an_object
    get "/process/#{process.object_id}/frames/1/objects/obj/objects"
    assert_equal "test2", json["@test2"]
    refute_nil json["@my_clone"]
    assert_match /TestObject/, json["(__self__)"]
  end

  def test_show_instance_variable
    get "/process/#{process.object_id}/frames/1/objects/obj/objects/@my_clone"
    assert_equal "test", json["@test"]
    assert_equal "test2", json["@test2"]
    assert_nil json["@my_clone"]
  end

  def test_delete_errors_from_log
    Maglev.expects(:abort_transaction).returns(true)
    Maglev.expects(:commit_transaction).returns(true)
    Maglev::System.expects(:needs_commit).returns(false)
    DebuggerLogEntry.create_continuation_labeled("deletable")
    size = ObjectLog.errors.size
    assert_match /deletable/, ObjectLog.errors.last.label
    delete "/process/#{ObjectLog.errors.last.object_id}"
    refute_match /deletable/, ObjectLog.errors.last.label if ObjectLog.errors.last
    assert ObjectLog.errors.size < size
  end

  def test_delete_process_killing_them
    assert process.alive?
    delete "/process/#{process.object_id}"
    refute process.alive?
  end

  def test_delete_pops_frames
    Maglev.expects(:abort_transaction).returns(true).twice
    get "/process/#{process.object_id}/frames/0"
    assert_equal "raise", json["method_name"]
    delete "/process/#{process.object_id}/frames/1"
    get "/process/#{process.object_id}/frames/0"
    assert_equal "thread_foo", json["method_name"]
  end

  def test_put_cannot_restart_errors
    DebuggerLogEntry.create_continuation_labeled("non-restartable")
    put "/process/#{ObjectLog.errors.last.object_id}"
    assert_equal 404, last_response.status
  end

  def test_put_cannot_restart_dead_process
    assert process.alive?
    process.exit
    refute process.alive?
    put "/process/#{process.object_id}"
    assert_equal 404, last_response.status
    @process = nil
  end

  def test_put_restarts_live_process
    assert process.stop?
    assert process.alive?
    process.__trim_stack_to_level(2) # Pop the raise, so resume will put us before that
    process[:skip_test_exception] = true # Skip throwing
    put "/process/#{process.object_id}"
    refute process.alive?
    assert process.stop?
  end

  def test_return_restarted_process_response
    assert process.stop?
    assert process.alive?
    process.__trim_stack_to_level(2) # Pop the raise, so resume will put us before that
    process[:skip_test_exception] = true # Skip throwing
    put "/process/#{process.object_id}"
    assert_equal process[:result].last.join, last_response.body
    @process = nil
  end

  def test_eval_in_frame
    skip "Evaluation in frame not implemented"
    post "/process/#{process.object_id}/frames/1", {"do-it" => "some"}
    assert_equal "context", json
  end

  def test_instance_eval_on_object
    @do_it = "@test"
    post "/process/#{process.object_id}/frames/1/objects/obj", {"do-it" => "@test"}
    assert_equal "test", json["(__self__)"]
  end

  def test_raise_exception_on_continued_failure
    assert process.stop?
    process.__trim_stack_to_level(2) # before the raise
    put "/process/#{process.object_id}"
    assert_equal 500, last_response.status
    process = nil
  end
end
