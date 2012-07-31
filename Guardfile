guard :shell do
  watch /.*\.l?hs$/ do |m|
    puts "Compiling..."
    `runghc #{m[0]} && echo "Compiled!"`
  end
end

# vim:ft=ruby
