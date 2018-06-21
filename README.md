Reprocessing Experiments
---

Playing with reprocessing to learn it and ReasonML. I'm implementing some physics with the goal of replicating something similar to Super Smash Bros Melee. 

## How to
```
git clone https://github.com/LaurencePutterman/reprocessing-experiments.git
```

### Install

```
npm install
```

### Build
```
npm run build
```

### Start
```
npm start
```

To build to JS run `npm run build:web` and then run a static server, like `python -m SimpleHTTPServer` and go to `localhost:8000`. If you're using safari you can simply open the `index.html` and tick `Develop > Disable Cross-Origin Restrictions`.

To build to native run `npm run build:native` and run `npm run start:native`

The build system used is [bsb-native](https://github.com/bsansouci/bsb-native).
