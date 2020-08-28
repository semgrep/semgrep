# re-open Ruby's Time class
class Time
  def yesterday
    self - 86400
  end
end

today = Time.now               # => 2013-09-03 16:09:37 +0300
yesterday = today.yesterday    # => 2013-09-02 16:09:37 +0300
