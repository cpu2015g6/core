s = ""
File.open(ARGV[0]) do |file|
	s = file.read
end
if s.length % 4 != 0
	STDERR.puts("Error!\n")
	exit(1)
end
print [s.length/4, 0].pack("N2")+s
