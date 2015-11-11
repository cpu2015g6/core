open(ARGV[0]){|f|
	i = 0
	while !f.eof
		puts (i.to_s + " => x\"" + f.read(4).unpack("H8")[0] + "\",")
		i=i+1
	end
}
