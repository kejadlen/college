lines = File.open("osil2").readlines

lines.each do |line|
  line.scan(/\(interpret-OSIL2 '(.*?) '(.*?)\) '(.*?)\)$/) do |match|
    match[0] = match[0].gsub('(','[').gsub(')',']').gsub(' ',',')
    match[1] = match[1].gsub('(','[').gsub(')',']').gsub(' ',',')
    match[2] = match[2].gsub('(','[').gsub(')',']').gsub(' ',',')
    puts "//=="
    puts "interpret_OSIL3(#{match[0]},#{match[1]},N)."
    puts "//--"
    puts "fail" if match[2] = "syntax-error"
    puts "[ N = match[2] ]" unless match[2] == "syntax-error"
  end
end
