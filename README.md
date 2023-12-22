## Part 1: Data modeling & Design

Notes:
See form-validation/FormValidation.purs for the code.
There's also a test suite provided.


The basic insight is to apply a technique called "Higher Kinded Data". The technique involves wrapping each field of the data type with an arbitrary functor. You can apply different functors to the data type to model different semantics. I first learned this technique from this blog: https://reasonablypolymorphic.com/blog/higher-kinded-data/


## Part 2: Dog Breed UI

This part is deployed to https://good-bois.netlify.app/

If you prefer to build and run locally:
```
# install dev tools, and purescript deps
npm install 

# compile the purescript
npm run build 

# start the dev server with vite
npm run watch:js 
```
