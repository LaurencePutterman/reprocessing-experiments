open Reprocessing;


module Player = {
  type parameters = {
    x: int,
    y: float,
    jump: bool,
    shortHop: bool,
    down: bool,
    left: bool,
    right: bool,
    yVelocity: float,
    jumpTicks: int,
    rotation: float,
    jumpStart: float,
  };

  let squareSize = 30;

  let gravity = -0.2;
  let jumpVelo = 20.0;
  let shortHopTiming = 0.01;

  let jump = (entity) => {
    let entity = if(entity.jump){
      {...entity, yVelocity: entity.yVelocity +. gravity *. float(entity.jumpTicks),jumpTicks: entity.jumpTicks+1}
    }else {
      {...entity, yVelocity: entity.yVelocity +. gravity *. float(entity.jumpTicks)}
    };
    entity;
  };

  let moveLeft = (entity, ~env) => {
    let entity = if(entity.left) {
      {...entity, x: entity.x - 10}
    } else {
      entity
    };
    entity
  };

  let moveRight = (entity, ~env) => {
    let entity = if(entity.right) {
      {...entity, x: entity.x + 10}
    } else {
      entity
    };
    entity
  };

  let takeInput = (entity, ~env) => {
    let entity = if(Env.keyPressed(W, env) && !entity.jump) {
      {...entity, jump: true, shortHop: false, yVelocity: jumpVelo, jumpTicks: 0, jumpStart: Sys.time()};
    } else {
      entity;
    };

    let entity = if(Env.keyReleased(W, env) && entity.jump) {
      let time = Sys.time();
      if(time -. entity.jumpStart <= shortHopTiming && !entity.shortHop) {
        {...entity, yVelocity: entity.yVelocity -. jumpVelo/.1.5, jumpTicks: entity.jumpTicks + 4, shortHop: true};
      } else {
        entity;
      };
    } else {
      entity;
    };

    let entity = if(Env.keyPressed(A, env)) {
      {...entity, left: true};
    } else {
      entity;
    };

    let entity = if(Env.keyReleased(A, env)) {
      {...entity, left: false};
    } else {
      entity;
    };

    let entity = if(Env.keyPressed(S, env)) {
      {...entity, down: true};
    } else {
      entity;
    };

    let entity = if(Env.keyReleased(S, env)) {
      {...entity, down: false};
    } else {
      entity;
    };

    let entity = if(Env.keyPressed(D, env)) {
      {...entity, right: true};
    } else {
      entity;
    };

    let entity = if(Env.keyReleased(D, env)) {
      {...entity, right: false};
    } else {
      entity;
    };

    entity
  };



  let drawPlayerGraphic = (entity, ~env) => {
    Draw.pushMatrix(env);
    Draw.fill(Utils.color(~r=41, ~g=166, ~b=244, ~a=255), env);
    Draw.translate(float(entity.x), entity.y, env);
    let entity = if(entity.jump) {
      Draw.rotate(entity.rotation, env);
      {...entity, rotation: entity.rotation +. 0.2};
    } else {
      {...entity, rotation: 0.0};
    };
    Draw.rect(~pos=(-squareSize/2, -squareSize/2), ~width=squareSize, ~height=squareSize, env);
    Draw.popMatrix(env);
    entity;
  };

  let handleMovement = (entity, ~env) =>
    entity
    |> moveRight(~env=env)
    |> moveLeft(~env=env);

  let drawEntity = (entity, ~env) =>
    entity
    |> handleMovement(~env=env)
    |> drawPlayerGraphic(~env=env);
};

let stageHeight = 100;
let windowWidth = 600;
let windowHeight = 600;

type stateT = {
  player: Player.parameters,
  /* entities: list(Entity.parameters), */
};



let setup = (env) => {
  Env.size(~width=windowWidth, ~height=windowHeight, env);
  {
    player: {
      x: 150,
      y: 150.0,
      jump: true,
      jumpTicks: 0,
      shortHop: false,
      down: false,
      left: false,
      right: false,
      yVelocity: 0.0,
      rotation: 0.0,
      jumpStart: 0.0,
    }
    /* entities: [{}] */
  };
};

let applyAcceleration = (entity, ~env) => {
  let entity = Player.jump(entity);
  let entity = if((entity.y -. entity.yVelocity +. float(Player.squareSize/2)) <= float(windowHeight - stageHeight)) {
    {...entity, y: entity.y -. entity.yVelocity}
  } else {
    {...entity, y: float(windowHeight - stageHeight - Player.squareSize/2), yVelocity: 0.0, jump: false}
  };
  entity
};

let drawStage = (state, env) => {
  Draw.fill(Utils.color(~r=0, ~g=0, ~b=0, ~a=255), env);
  Draw.rect(~pos=(0, windowHeight-stageHeight), ~width=windowWidth, ~height=stageHeight, env);
  state;
};

let handlePlayer = (entity, env) =>
  entity
  |> applyAcceleration(~env=env)
  |> Player.takeInput(~env=env)
  |> Player.drawEntity(~env=env);


let draw = (state, env) => {
  Draw.background(Utils.color(~r=199, ~g=217, ~b=229, ~a=255), env);
  let state = drawStage(state, env);
  let state = {...state, player: handlePlayer(state.player, env)};
  state;
};



run(~setup, ~draw, ());
