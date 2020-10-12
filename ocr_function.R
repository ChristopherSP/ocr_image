library(tesseract)
library(magick)
library(stringi)
library("EBImage")
library(exifr)

# Preprocessing image for OCR
processingImage = function(path = "", gamma = 0.5, contrast = 1.15, brightness = 0, brushShape = "diamond", brushSize = 5){
  # OCR direct from image
  tesseract(language = "por")
  inputImage = image_read(path)
  imageExif = read_exif(path)
  orientation = imageExif$Orientation
  # if(image_info(inputImage)$`width` > image_info(inputImage)$`height`){
  #   inputImage = image_rotate(inputImage, 90)
  # }
  
  if(orientation%in%c(2,4,5,7)){
    inputImage = image_flop(inputImage)
    orientation = ifelse(orientation%%2 == 0, orientation -1, orientation + 1)
  }
  
  switch(as.character(orientation),
    "3" = inputImage <- image_rotate(inputImage, 180),
    "6" = inputImage <- image_rotate(inputImage, -90),
    "8" = inputImage <- image_rotate(inputImage, 90))
  
  kern = makeBrush(brushSize, shape=brushShape)
  
  outputImage = as_EBImage(inputImage)
  
  colorMode(outputImage) = Grayscale
  
  outputImage = erode(((
    outputImage^gamma
        )*contrast
      ) + brightness,
    kern)

  extention = strsplit(path)[[1]]
  extention = extention[length(extention)]
  
  outputText = ocr(image_read(outputImage))
  
  # write.table(outputText, stri_replace_last_fixed(path, extention, "txt"), row.names = F, col.names = F, quote = F, sep = "\t")
  gc()
}

preprocessingImage("~/Downloads/contrato.jpg")

# contratoBW = image_enhance(image_quantize(image_modulate(image_contrast(contratoJPG, sharpen = 1), brightness = 130, saturation = 0, hue = 100), max = 2, colorspace = "gray"))

