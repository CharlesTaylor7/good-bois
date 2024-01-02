## How to Demo

The app is live at: https://good-bois.netlify.app/

If you prefer to build and run locally:

```bash
# install dev tools, and purescript deps
npm install 

# compile the purescript
npm run build 

# start the dev server with vite
npm run watch:js 
```


### Notes
This dog image viewer, is built with Halogen. It has 3 main components, all defined in the `src/DogCeo/Components/` directory.

#### App Component
This component renders no html directly.
It performing api calls, and caching previous calls to the breeds or images api. 
It tracks the current page and delegates rendering to the relevant child component.

#### Breeds Component
This component lists all breeds and sub breeds in alphabetical order.

#### Images Component
This component is the actual image viewer, that handles paginating though images and displaying them.

Some of the image urls returned by the dog.ceo API are dead links. 
If a link is dead, the image is replaced with a custom error image.
( You can see examples of this on the first page of pugs. )

While an image is loading, a placeholder loading gif is shown in its place.
Since images do not load instantly and can load out of order, this helps reduce jarring  "popping" and "sliding" of elements.
