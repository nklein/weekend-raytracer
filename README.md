# WEEKEND-RAYTRACER

This work implements an N-dimensional version of Peter Shirley's, Trevor David Black's, and Steve Hollasch's
raytracer from their [Raytracing in One Weekend Book Series][book].

  [book]: https://raytracing.github.io/books/RayTracingInOneWeekend.html

## Examples

### 1.2.2: Book 1, Chapter 2, Section 2: Creating an Image File

This implementation outputs PNG files rather than PPM files.
Additionally, it supports outputting (N-1)-dimensional image cubes and
interleaving the various slices with borders between them.

![Image cube](./images/B1C2-2image.png)

You can output an image akin to Book 1, Chapter 2, Section 2's image
with the following function:

    (weekend-raytracer/examples:b1c2-2image &optional verticalp)

The image here is a 3-D image cube rendered as horizontal slices of
constant blueness (or as vertical slices of constant blueness if
`VERTICALP` is non-`NIL`). The first slice is identical to the
book image.

### 1.4.2: Book 1, Chapter 4, Section 2: Creating an Image File

The book image here renders an interpolation between blue and
white. This image renders between blue and white but also between
orange and the color the book would render.

![Image cube](./images/B1C4-2image.png)

You can output an image akin to Book 1, Chapter 4, Section 2's image
with the following function:

    (weekend-raytracer/examples:b1c4-2image &optional verticalp)

The image here is a 3-D image cube rendered as horizontal slices.
The color of each pixel is based on the 3rd and 4th coordinates
of the camera ray's direction.


### 1.5.2: Book 1, Chapter 5, Section 2: Creating Our First Raytraced Image

The book image here renders an interpolation between blue and
white with a red sphere in the center of the view.

The book has the image plane in the negative-z direction and
then uses x and y for coordinates within the image square.

For maximum utility when changing number of dimensions, this
code puts the image plane in the negative-x direction and
then uses y, z, and w to move around the image cube.

I have moved the sphere slightly in the w direction.

![Image cube](./images/B1C5-2image.png)

You can output an image akin to Book 1, Chapter 5, Section 2's image
with the following function:

    (weekend-raytracer/examples:b1c5-2image &optional verticalp)

The image here is a 3-D image cube rendered as horizontal slices.
The color of each pixel is based on the 3rd and 4th coordinates
of the camera ray's direction except where the ray intersects the
sphere. Where the ray intersects the sphere, the image is solid red.


### 1.6.1: Book 1, Chapter 6, Section 1: Shading With Surface Normals

The book image here renders an interpolation between blue and
white with a sphere in the center of the view.
The colors on the sphere are a function of the normal on the
sphere at the point of intersection.

The book has the image plane in the negative-z direction and
then uses x and y for coordinates within the image square.

For maximum utility when changing number of dimensions, this
code puts the image plane in the negative-x direction and
then uses y, z, and w to move around the image cube.

I have moved the sphere slightly in the w direction.

![Image cube](./images/B1C6-1image.png)

You can output an image akin to Book 1, Chapter 6, Section 1's image
with the following function:

    (weekend-raytracer/examples:b1c6-1image &optional verticalp)

The image here is a 3-D image cube rendered as horizontal slices.
The color of each pixel is based on the 3rd and 4th coordinates
of the camera ray's direction except where the ray intersects the
sphere. Where the ray intersects the sphere, the color is

    r = (/ (1+ (vref normal 1)) 2)
    g = (/ (1+ (vref normal 2)) 2)
    b = (/ (1+ (vref normal 0)) 2)


### 1.6.1: Book 1, Chapter 6, Section 7: Common Constants And Utility Functions

The book image here renders an interpolation between blue and
white with a sphere in the center of the view and another sphere as the ground.
The colors on the sphere are a function of the normal on the
sphere at the point of intersection.

The book has the image plane in the negative-z direction and
then uses x and y for coordinates within the image square.

For maximum utility when changing number of dimensions, this
code puts the image plane in the negative-x direction and
then uses y, z, and w to move around the image cube.

I have moved both spheres slightly in the w direction.

![Image cube](./images/B1C6-7image.png)

You can output an image akin to Book 1, Chapter 6, Section 7's image
with the following function:

    (weekend-raytracer/examples:b1c6-7image &optional verticalp)

The image here is a 3-D image cube rendered as horizontal slices.
The color of each pixel is based on the 3rd and 4th coordinates
of the camera ray's direction except where the ray intersects the
sphere. Where the ray intersects the spheres, the color is

    r = (/ (1+ (vref normal 1)) 2)
    g = (/ (1+ (vref normal 2)) 2)
    b = (/ (1+ (vref normal 0)) 2)

### 1.7.1: Book 1, Chapter 7, Section 1: Moving Camera Code Into Its Own Class

This is essentially the same as image 1.6.7 above.
The difference here is that the camera is moved into its own class.
The sky color is also not being LERP-ed at the moment as a function
of the normal to simplify allowing the camera to take in arbitrary
numbers of spatial and color dimensions.

![Image cube](./images/B1C7-1image.png)

You can generate this image with the following function:

    (weekend-raytracer/examples:b1c7-1image &optional verticalp)

The image here is a 3-D image cube rendered as horizontal slices.
The color of each pixel is the default sky color except where the ray
intersects the sphere.
Where the ray intersects the spheres, the color is

    r = (/ (1+ (vref normal 1)) 2)
    g = (/ (1+ (vref normal 2)) 2)
    b = (/ (1+ (vref normal 0)) 2)

### 1.8.2: Book 1, Chapter 8, Section 2: Generating Pixels With Multiple Samples

This is essentially the same as image 1.7.1 above except with anti-aliasing.

![Image cube](./images/B1C8-2image.png)

You can generate this image with the following function:

    (weekend-raytracer/examples:b1c8-2image samples-per-pixel &optional verticalp)

The current anti-aliasing takes the aspect ratios into consideration when
deciding how far to stray from the center ray of a pixel.
Without this, the fact that some axises have hundreds of pixels along them
and others only have a few pixels along them
means that pixel cubes are really-long along some axises
making it a great deal easier to hit or miss objects in some directions.
That results in very fuzzy edges unless you bump the number of samples up by many orders of magnitude.

This means the simulated camera has roughly (hyper)cubical sensors at each pixel
but that the pixels are spaced out more on axises where there are fewer pixels per degree of view.
