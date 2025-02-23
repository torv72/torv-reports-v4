# Test available fonts
print("Starting font check")
available_fonts <- systemfonts::system_fonts()
print("Number of fonts found:")
print(nrow(available_fonts))
print("Checking for Georama:")
print(sum(grepl("Georama", available_fonts$family)))



















