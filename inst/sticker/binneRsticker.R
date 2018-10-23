library(hexSticker)
library(magick)
library(showtext)

font_add_google("Ubuntu", "ubuntu")

imgurl <- image_read("inst/sticker/binneR.png")
sticker(imgurl, package = "binneR", 
				h_fill = "white",
				h_color = 'black',
				p_x = 1,
				p_y = 0.4,
				p_size = 18, 
				p_color = "black",
				p_family = "ubuntu",
				s_x = 1, 
				s_y = 1.2, 
				s_width = 1.1,
				s_height = 1.1,
				filename = "inst/sticker/binneRsticker.png")
