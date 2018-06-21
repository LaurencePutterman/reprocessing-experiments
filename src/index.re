open Reprocessing;

type direction = Left | Right;
type force = {
  angle: float, /*0 degrees is right, 90 is up, etc */
  magnitude: float,
  mutable currTicks: int,
  maxTicks: int
};

module Player = {
  type parameters = {
    x: float,
    y: float,
    forces: list(force),
    jump: bool,
    shortHop: bool,
    down: bool,
    left: bool,
    right: bool,
    yVelocity: float,
    xVelocity: float,
    rotation: float,
    jumpStart: float,
    punch: bool,
    punchTicks: int,
    facing: direction
  };

  let squareSize = 30;

  let gravity = -0.4;
  let jumpVelo = 10.0;
  let shortHopTiming = 0.1;

  let applyForces = (entity, ~env) => {
    let xVelocity= entity.xVelocity +. List.fold_left((r, elem) => r +. (cos(Utils.radians(elem.angle)) *. elem.magnitude), 0.0, entity.forces);
    let yVelocity= entity.yVelocity +. List.fold_left((r, elem) => r +. (sin(Utils.radians(elem.angle)) *. elem.magnitude), 0.0, entity.forces);
    let apply = (f) => {
      f.currTicks = f.currTicks + 1;
    };
    List.iter(apply, entity.forces);
    let forces = List.filter((f) => f.currTicks < f.maxTicks ,entity.forces);
    let entity = {...entity, yVelocity: yVelocity +. gravity, xVelocity: xVelocity, forces: forces};
    entity;
  };
  let moveLeft = (entity, ~env) => {
    let entity = if(entity.left) {
      if(!entity.jump) {
        {...entity, x: entity.x -. 10., facing: Left}
      } else {
        {...entity, x: entity.x -. 10.}
      };
    } else {
      entity
    };
    entity
  };
  let moveRight = (entity, ~env) => {
    let entity = if(entity.right) {
      if(!entity.jump) {
        {...entity, x: entity.x +. 10., facing: Right}
      } else {
        {...entity, x: entity.x +. 10.}
      };
    } else {
      entity
    };
    entity
  };
  let takeInput = (entity, ~env) => {
    /* let entity = if(Env.keyPressed(W, env) && !entity.jump) {
      {...entity, jump: true, shortHop: false, yVelocity: jumpVelo, jumpTicks: 0, jumpStart: Sys.time()};
    } else {
      entity;
    }; */
    let entity = if(Env.keyPressed(W, env) && !entity.jump) {
      {...entity, jump:true, forces: [{angle: 90., magnitude: jumpVelo, currTicks: 0, maxTicks:1}, ...entity.forces] };
    } else {
      entity;
    };

    /* let entity = if(Env.keyReleased(W, env) && entity.jump) {
      let time = Sys.time();
      if(time -. entity.jumpStart <= shortHopTiming && !entity.shortHop) {
        {...entity, yVelocity: entity.yVelocity -. jumpVelo/.1.5, jumpTicks: entity.jumpTicks + 2, shortHop: true};
      } else {
        entity;
      };
    } else {
      entity;
    }; */

    let entity = if(Env.keyPressed(L, env)) {
      {...entity, punch: true};
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


  let drawPlayerProjectiles = (entity, ~env) => {
    Draw.pushMatrix(env);
    Draw.translate(entity.x, entity.y, env);
    let entity = if(entity.punch && entity.punchTicks < 6) {
      Draw.fill(Utils.color(~r=0, ~g=0, ~b=0, ~a=255), env);
      switch (entity.facing) {
        | Left => Draw.ellipse(~center=(squareSize/8 - (entity.punchTicks *10), squareSize/8), ~radx=10, ~rady=10, env)
        | Right => Draw.ellipse(~center=(squareSize/8 + (entity.punchTicks *10), squareSize/8), ~radx=10, ~rady=10, env);
      };
      {...entity, punchTicks: entity.punchTicks+1};
    } else {
      {...entity, punchTicks: 0, punch: false};
    };
    Draw.popMatrix(env);
    entity
  };

  let drawPlayerGraphic = (entity, ~env) => {
    Draw.pushMatrix(env);
    Draw.fill(Utils.color(~r=41, ~g=166, ~b=244, ~a=255), env);
    Draw.translate(entity.x, entity.y, env);
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
    |> drawPlayerGraphic(~env=env)
    |> drawPlayerProjectiles(~env=env);
};
module Enemy = {
  type parameters = {
    x: float,
    y: float,
    forces: list(force),
    jump: bool,
    shortHop: bool,
    down: bool,
    left: bool,
    right: bool,
    yVelocity: float,
    xVelocity: float,
    rotation: float,
    jumpStart: float,
    punch: bool,
    punchTicks: int,
    facing: direction
  };

  let squareSize = 30;

  let gravity = -0.4;
  let friction = -0.04;
  let jumpVelo = 10.0;
  let shortHopTiming = 0.1;

  let receiveForces = (entity, ~env, ~player: Player.parameters) => {
    let didCollide = switch (player.facing) {
            | Right => Utils.intersectRectCircle(~rectPos=(entity.x, entity.y), ~rectW=float(squareSize), ~rectH=float(squareSize), ~circlePos=(player.x -. float(squareSize/8 - (player.punchTicks *10)), player.y -. float(squareSize/8)), ~circleRad=10.)
            | Left => Utils.intersectRectCircle(~rectPos=(entity.x, entity.y), ~rectW=float(squareSize), ~rectH=float(squareSize), ~circlePos=(player.x -. float(squareSize/8 + (player.punchTicks *10)), player.y -. float(squareSize/8)), ~circleRad=10.);
          };
    let entity = if(player.punch && player.punchTicks < 6 && didCollide) {
      let entity = switch (player.facing) {
        | Right => {...entity, forces: [{angle: 110., magnitude: 2., currTicks: 0, maxTicks:1}, ...entity.forces] }
        | Left => {...entity, forces: [{angle: 70., magnitude: 2., currTicks: 0, maxTicks:1}, ...entity.forces] };
      };
      entity;
    } else {
      entity;
    };
    entity;
  };

  let applyForces = (entity, ~env) => {
    let xVelocity= entity.xVelocity +. List.fold_left((r, elem) => r +. (cos(Utils.radians(elem.angle)) *. elem.magnitude), 0.0, entity.forces);
    let yVelocity= entity.yVelocity +. List.fold_left((r, elem) => r +. (sin(Utils.radians(elem.angle)) *. elem.magnitude), 0.0, entity.forces);
    let apply = (f) => {
      f.currTicks = f.currTicks + 1;
    };
    List.iter(apply, entity.forces);
    let forces = List.filter((f) => f.currTicks < f.maxTicks ,entity.forces);
    let xVelocity = if (xVelocity > 0.0 && xVelocity +. friction > 0.0) {
      (xVelocity +. friction);
    } else {
      if(xVelocity < 0.0 && xVelocity -. friction < 0.0) {
        (xVelocity -. friction);
      } else {
        0.0;
      };
    };

    let entity = {...entity, yVelocity: yVelocity +. gravity, xVelocity: xVelocity, forces: forces};
    entity;
  };
  let moveLeft = (entity, ~env) => {
    let entity = if(entity.left) {
      if(!entity.jump) {
        {...entity, x: entity.x -. 10., facing: Left}
      } else {
        {...entity, x: entity.x -. 10.}
      };
    } else {
      entity
    };
    entity
  };
  let moveRight = (entity, ~env) => {
    let entity = if(entity.right) {
      if(!entity.jump) {
        {...entity, x: entity.x +. 10., facing: Right}
      } else {
        {...entity, x: entity.x +. 10.}
      };
    } else {
      entity
    };
    entity
  };

  let drawPlayerGraphic = (entity, ~env) => {
    Draw.pushMatrix(env);
    Draw.fill(Utils.color(~r=241, ~g=166, ~b=244, ~a=255), env);
    Draw.translate(entity.x, entity.y, env);
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
  enemy: Enemy.parameters
  /* entities: list(Entity.parameters), */
};



let setup = (env) => {
  Env.size(~width=windowWidth, ~height=windowHeight, env);
  {
    player: {
      x: 150.0,
      y: 150.0,
      jump: true,
      shortHop: false,
      down: false,
      left: false,
      right: false,
      yVelocity: 0.0,
      xVelocity: 0.0,
      rotation: 0.0,
      jumpStart: 0.0,
      punch: false,
      punchTicks: 0,
      facing: Right,
      forces: [],
    },
    enemy: {
      x: 300.0,
      y: 150.0,
      jump: true,
      shortHop: false,
      down: false,
      left: false,
      right: false,
      yVelocity: 0.0,
      xVelocity: 0.0,
      rotation: 0.0,
      jumpStart: 0.0,
      punch: false,
      punchTicks: 0,
      facing: Right,
      forces: [],
    }
    /* entities: [{}] */
  };
};

let applyVelocities = (entity: Player.parameters, ~env) => {
  let entity = if((entity.y -. entity.yVelocity +. float(Player.squareSize/2)) <= float(windowHeight - stageHeight)) {
    {...entity, y: entity.y -. entity.yVelocity, x: entity.x -. entity.xVelocity}
  } else {
    {...entity, y: float(windowHeight - stageHeight - Player.squareSize/2), yVelocity: 0.0, x: entity.x -. entity.xVelocity, jump: false}
  };
  entity
};

let applyEnemyVelocities = (entity: Enemy.parameters, ~env) => {
  let entity = if((entity.y -. entity.yVelocity +. float(Enemy.squareSize/2)) <= float(windowHeight - stageHeight)) {
    {...entity, y: entity.y -. entity.yVelocity, x: entity.x -. entity.xVelocity}
  } else {
    {...entity, y: float(windowHeight - stageHeight - Enemy.squareSize/2), yVelocity: 0.0, x: entity.x -. entity.xVelocity, jump: false}
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
  |> Player.applyForces(~env=env)
  |> applyVelocities(~env=env)
  |> Player.takeInput(~env=env)
  |> Player.drawEntity(~env=env);

let handleEnemy = (entity: Enemy.parameters, env, player: Player.parameters) =>
  entity
  |> Enemy.receiveForces(~env=env, ~player=player)
  |> Enemy.applyForces(~env=env)
  |> applyEnemyVelocities(~env=env)
  |> Enemy.drawEntity(~env=env);

let draw = (state, env) => {
  Draw.background(Utils.color(~r=199, ~g=217, ~b=229, ~a=255), env);
  let state = drawStage(state, env);
  let state = {...state,
    player: handlePlayer(state.player, env),
    enemy: handleEnemy(state.enemy, env, state.player)
  };
  state;
};



run(~setup, ~draw, ());
