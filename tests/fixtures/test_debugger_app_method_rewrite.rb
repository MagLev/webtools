class DebuggerAppMethodRewrite
  def holohoop
    raise Exception
  end
  
  def rewritten?
    holohoop
    false
  end
end
