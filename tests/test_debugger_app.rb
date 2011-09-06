ENV["RACK_ENV"] = "test"
require 'minitest/autorun'
require 'rack/test'
require 'web_tools'
require 'mocha'
require 'fileutils'

class DebuggerTest < MiniTest::Unit::TestCase
  Fixtures = { :method_rewrite => File.expand_path("../fixtures/test_debugger_app_method_rewrite.rb", __FILE__) }
  Fixtures.values.each {|f| require f }
  
  include Rack::Test::Methods
  WebTools::Debugger

  def app
    WebTools::Debugger
  end

  class TestObject
    attr_reader :test, :test2, :my_clone

    def inspect
      "inspected TestObject"
    end
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
    assert_match /TestObject/, json["self"]
    assert_equal TestObject.new.inspect, json["inspect"]
    assert_equal TestObject.name, json["class"]
    assert_equal "test", json["instance_variables"]["@test"]
    assert_equal "test2", json["instance_variables"]["@test2"]
    assert_match /TestObject/, json["instance_variables"]["@my_clone"]
  end

  def test_returns_a_1_level_hash_with_the_instance_variables_and_the_SELF_key
    get "/process/#{process.object_id}/frames/1/objects/obj"
    assert_match /TestObject/, json["self"]
    refute_nil json["instance_variables"]
  end

  def test_inspect_into_an_objects_instance_variables
    get "/process/#{process.object_id}/frames/1/objects/obj/objects"
    assert_equal "test2", json["@test2"]
    refute_nil json["@my_clone"]
    assert(json.keys.all? {|o| o.start_with? "@"})
  end

  def test_show_instance_variable
    get "/process/#{process.object_id}/frames/1/objects/obj/objects/@my_clone"
    assert_equal "test", json["instance_variables"]["@test"]
    assert_equal "test2", json["instance_variables"]["@test2"]
    assert_nil json["instance_variables"]["@my_clone"]
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

  def test_post_cannot_restart_errors
    DebuggerLogEntry.create_continuation_labeled("non-restartable")
    post "/process/#{ObjectLog.errors.last.object_id}", {"running" => "true"}
    assert_equal 404, last_response.status
  end

  def test_post_cannot_restart_dead_process
    assert process.alive?
    process.exit
    refute process.alive?
    post "/process/#{process.object_id}", {"running" => "true"}
    assert_equal 404, last_response.status
    @process = nil
  end

  def test_post_restarts_live_process
    assert process.stop?
    assert process.alive?
    process.__trim_stack_to_level(2) # Pop the raise, so resume will put us before that
    process[:skip_test_exception] = true # Skip throwing
    post "/process/#{process.object_id}", {"running" => "true"}
    refute process.alive?
    assert process.stop?
  end

  def test_return_restarted_process_response
    assert process.stop?
    assert process.alive?
    process.__trim_stack_to_level(2) # Pop the raise, so resume will put us before that
    process[:skip_test_exception] = true # Skip throwing
    post "/process/#{process.object_id}", {"running" => "true"}
    assert_equal process[:result].last.join, last_response.body
    @process = nil
  end

  def test_eval_in_frame
    skip
    put "/process/#{process.object_id}/frames/1", {"do-it" => "some"}
    assert_equal "context", json["do-it-result"]["self"]
    assert_equal "context".inspect, json["do-it-result"]["inspect"]
    assert_equal "String", json["do-it-result"]["class"]
  end

  def test_step_into_frame
    skip
    assert process.stop?
    assert process.alive?
    into = Maglev::Debugger::Frame.new(:method => process.__method_at(1),
                                       :thread => process,
                                       :index => 1)
    above = Maglev::Debugger::Frame.new(:method => process.__method_at(2),
                                        :thread => process,
                                        :index => 2)
    above.delete # Pop before that
    put("/process/#{process.object_id}/frames/0", "index" => 1)
    new_top = Maglev::Debugger::Frame.new(:method => process.__method_at(1),
                                          :thread => process,
                                          :index => 1)
    assert_equal into.method_name, new_top.method_name
    @process = nil
  end

  def test_step_in_frame
    skip
    assert process.stop?
    assert process.alive?
    above = Maglev::Debugger::Frame.new(:method => process.__method_at(2),
                                        :thread => process,
                                        :index => 2)
    prev_name = above.method_name
    above.delete # Pop before that
    above = Maglev::Debugger::Frame.new(:method => process.__method_at(1),
                                        :thread => process,
                                        :index => 1)
    assert_equal 1, above.debug_info![:stepOffset]

    put("/process/#{process.object_id}/frames/0",
        "debug_info" => { "stepOffset" => 2 })

    above = Maglev::Debugger::Frame.new(:method => process.__method_at(1),
                                        :thread => process,
                                        :index => 1)
    assert_equal prev_name, above.method_name
    assert_equal 2, above.debug_info![:stepOffset]

    @process = nil
  end

  def test_change_source_code
    res = Maglev::Debugger.debug do
      DebuggerAppMethodRewrite.new.rewritten?
    end
    Thread.pass
    @process = res[:result].thread
    assert @process.alive?
    assert @process.stop?
    new_source = "  def rewritten?\n    true\n  end\n"
    FileUtils.cp(Fixtures[:method_rewrite], Fixtures[:method_rewrite] + ".bak")

    put "/process/#{process.object_id}/frames/2", {
      "debug_info" => {
        "source" => new_source } }

    top = Maglev::Debugger::Frame.new(:method => @process.__method_at(1),
                                     :thread => @process,
                                     :index => 1)
    top.debug_info!
    assert_equal :rewritten?, top.method_name
    assert_equal new_source, top.debug_info[:source]
  ensure
    FileUtils.cp(Fixtures[:method_rewrite] + ".bak", Fixtures[:method_rewrite])
  end

  def test_instance_eval_on_object
    @do_it = "@test"
    put "/process/#{process.object_id}/frames/1/objects/obj", {"do-it" => "@test"}
    assert_equal "@test", json["do-it"]
    assert_equal "test", json["do-it-result"]["self"]
    assert_equal "test".inspect, json["do-it-result"]["inspect"]
    assert_equal "String", json["do-it-result"]["class"]
  end

  def test_raise_exception_on_continued_failure
    assert process.stop?
    process.__trim_stack_to_level(2) # before the raise
    post "/process/#{process.object_id}", "running" => "true"
    assert_equal 500, last_response.status
    @process = nil
  end
end
