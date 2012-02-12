class Hash
  def sort_by_value
    sort {|a,b| b[1]<=>a[1]}
  end
end

class Cipher
  attr_accessor :substitutions, :ciphertext

  @@frequencies = {
    "a" => 0.082, "b" => 0.015, "c" => 0.028, "d" => 0.043, "e" => 0.127,
    "f" => 0.022, "g" => 0.020, "h" => 0.061, "i" => 0.070, "j" => 0.002,
    "k" => 0.008, "l" => 0.040, "m" => 0.024, "n" => 0.067, "o" => 0.075,
    "p" => 0.019, "q" => 0.001, "r" => 0.060, "s" => 0.063, "t" => 0.091,
    "u" => 0.028, "v" => 0.010, "w" => 0.023, "x" => 0.001, "y" => 0.020,
    "z" => 0.001
  }

  @@common_digrams = [
    "th", "he", "in", "er", "an", "re", "ed", "on", "es", "st",
    "en", "at", "to", "nt", "ha", "nd", "ou", "ea", "ng", "as",
    "or", "ti", "is", "et", "it", "ar", "te", "se", "hi", "of"
  ]
  
  @@common_trigrams = [
    "the", "ing", "and", "her", "ere", "ent",
    "tha", "nth", "was", "eth", "for", "dth"
  ]
  
  def initialize(text)
    @ciphertext = text.delete " \n"
    @substitutions = Hash.new
  end
  
  def frequency(text = @ciphertext, n = 1)
    h = Hash.new(0)
    0.upto(text.size-n) do |i|
      h[text[i,n]] += 1
    end
    h
  end
  
  def substitute(text = @ciphertext, subs = @substitutions)
    str = text.dup
    subs.each do |key, value|
      str.gsub!(key, value)
    end
    str
  end

  def guess
    letters = @@frequencies.sort_by_value.map {|i|
      i[0]}.reject {|i|
      @substitutions.has_value? i}[0,10]
  end
  
end  

t = Cipher.new <<CIPHER
    EMGLOSUDCGDNCUSWYSFHNSFCYKDPUMLWGYICOXYSIPJCK
    QPKUGKMGOLICGINCGACKSNISACYKZSCKXECJCKSHYSXCG
    OIDPKZCNKSHICGIWYGKKGKGOLDSILKGOIUSIGLEDSPWZU
    GFZCCNDGYYSFUSZCNXEOJNCGYEOWEUPXEZGACGNFGLKNS
    ACIGOIYCKXCJUCIUZCFZCCNDGYYSFEUEKUZCSOCFZCCNC
    IACZEJNCSHFZEJZEGMXCYHCJUMGKUCY 
CIPHER

p t.frequency.sort_by_value
t.substitutions["F"] = "w"
t.substitutions["C"] = "e"
t.guess
p t.substitute

# p frequency(ciphertext, 1).sort_by_value[0,10]
# p frequency(ciphertext, 2).sort_by_value[0,10]
# p frequency(ciphertext, 3).sort_by_value[0,10]
# puts "-"
# p frequency(substitute(ciphertext, substitutions), 2).select {|k,v| k.include? "w"}.sort {|a,b| b[1]<=>a[1]}
# p frequency(substitute(ciphertext, substitutions), 3).select {|k,v| k.include? "w"}.sort {|a,b| b[1]<=>a[1]}
# p substitute(ciphertext, substitutions)
