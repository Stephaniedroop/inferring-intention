
var world;
var bodies = []; // instances of b2Body (from Box2D)

var preGlobalPos = null; //What is this for??

var maxSpeed = 30; //A cap on the speed of the objects so things don't get out of control.  (10 means 10 'meters' per second??)
var stopTime = 2700;
var frameNum = 0

var xPos = 0;
var yPos = 0;

var damping;
var ctrl_obj_damping;

var data = {
  timeline: [],
  events: [],
  setup: []
};

//console.log('hello world');

//Declaring a bunch of needed box2d variables
var b2Vec2 = Box2D.Common.Math.b2Vec2,
  b2BodyDef = Box2D.Dynamics.b2BodyDef,
  b2Body = Box2D.Dynamics.b2Body,
  b2FixtureDef = Box2D.Dynamics.b2FixtureDef,
  b2World = Box2D.Dynamics.b2World,
  b2PolygonShape = Box2D.Collision.Shapes.b2PolygonShape;
b2CircleShape = Box2D.Collision.Shapes.b2CircleShape;
b2GravityController = Box2D.Dynamics.Controllers.b2GravityController;
b2TensorDampingController = Box2D.Dynamics.Controllers.b2TensorDampingController;
b2ContactListener = Box2D.Dynamics.b2ContactListener;




function Start(cond) {


  initial_pixel_ratio = original_paths['condition_details']['pixel_ratio'];//window.devicePixelRatio;
  ratio = original_paths['condition_details']['box2d_ratio']; //* initial_pixel_ratio; //TODO: Not sure if we should multiply by pixel ratio here!
  
  //console.log('ratio', ratio);
  //console.log('inital_pixel_ratio', initial_pixel_ratio, 'ratio', ratio);
  //possWorld = cond[0]; //STATE.index;

    fullX = 6//stage.stageWidth / ratio;
    fullY = 4//stage.stageHeight / ratio;

    xPos = ratio * fullX / 2;
    yPos = ratio * fullY / 2;
    stepSize = 1 / 60; //120;


  ////////////////////////////////
  //Read in specifiable properties
  ////////////////////////////////

  //learn_cond = cond[0];

  local_forces = [
    [0, cond[0], cond[1][0], cond[1][1]],
    [0, 0, cond[1][2], cond[1][3]],
    [0, 0, 0, cond[1][4]],
    [0, 0, 0, 0]
  ];
  var o_mass = cond[2];

  /////////////////////////////
  //Set fixed/randomised properties
  /////////////////////////////

  ctrl_obj_damping = Number(original_paths['condition_details']['ctrl_obj_damping']); //Controlled object damping
  damping = Number(original_paths['condition_details']['o_damping']); //Uncontrolled object damping
  
  var wall_elasticity = Number(original_paths['condition_details']['wall_elasticity']);//.98; //Sets elasticity
  var wall_friction = Number(original_paths['condition_details']['wall_friction']);//0.05; //Sets friction of the walls  
  var wall_mass = Number(original_paths['condition_details']['wall_mass']);
  var wall_damping = Number(original_paths['condition_details']['wall_damping']);
  var o_friction = Number(original_paths['condition_details']['o_friction']);
  var o_elasticity = Number(original_paths['condition_details']['o_elasticity']);
  //console.log('input vars', ctrl_obj_damping, ' ', damping, ' ', wall_elasticity, ' ', wall_friction, ' ', 
  //wall_mass, ' ', wall_damping, ' ', o_friction, ' ', o_elasticity);
  
  ////////////////////////////////////
  //Things that are reset on each trial
  /////////////////////////////////////

  world = new b2World(new b2Vec2(0, 0));

  

  /////////////////////////////////////////////
  //Create the walls, set them as static bodies
  /////////////////////////////////////////////

  borderWidth = Math.min(fullY / 20, fullX / 20);

  //Parameters: (w,      h,          x,         y,        type, density, damping, friction, restitution, userData, isRendered, stage, img)
  createBox(fullX / 2, borderWidth, fullX / 2, 0, b2Body.b2_staticBody, wall_mass, wall_damping, wall_elasticity, wall_friction, {
    name: "top_wall",
    bodyType: "static",
    W: fullX / 2,
    H: borderWidth
  });
  createBox(fullX / 2, borderWidth, fullX / 2, fullY, b2Body.b2_staticBody, wall_mass, wall_damping, wall_elasticity, wall_friction, {
    name: "bottom_wall",
    bodyType: "static",
    W: fullX / 2,
    H: borderWidth
  });
  createBox(borderWidth, fullY / 2, 0, fullY / 2, b2Body.b2_staticBody, wall_mass, wall_damping, wall_elasticity, wall_friction, {
    name: "left_wall",
    bodyType: "static",
    W: borderWidth,
    H: fullY / 2
  });
  createBox(borderWidth, fullY / 2, fullX, fullY / 2, b2Body.b2_staticBody, wall_mass, wall_damping, wall_elasticity, wall_friction, {
    name: "right_wall",
    bodyType: "static",
    W: borderWidth,
    H: fullY / 2
  });

   
  ///////////////////////////
  //Create the mobile objects
  ///////////////////////////

  ball_radius = Math.min(fullX / 12, fullY / 12); //Math.min(fullX / 16, fullY / 16);
   
   
  //Starting locations
  //TODO GIVE THEM STARTING LOCATIONS AND VELOCITIES ACCORDING TO THE FRAMES WE ARE INTERESTED IN
  sc_locs = [[original_paths["o1.x"][0],original_paths["o1.y"][0]],
               [original_paths["o2.x"][0],original_paths["o2.y"][0]],
               [original_paths["o3.x"][0],original_paths["o3.y"][0]],
               [original_paths["o4.x"][0],original_paths["o4.y"][0]]];
  //var sc_locs = RandomLocations(4);

  //console.log('starting locs', sc_locs);
  //Starting velocities
  var svs = [new b2Vec2((original_paths["o1.vx"][0]), (original_paths["o1.vy"][0])),
     new b2Vec2((original_paths["o2.vx"][0]), (original_paths["o2.vy"][0])),
     new b2Vec2((original_paths["o3.vx"][0]), (original_paths["o3.vy"][0])),
     new b2Vec2((original_paths["o4.vx"][0]), (original_paths["o4.vy"][0]))
   ];


  //console.log('starting velocities', svs);
  //console.log('o_mass', o_mass);
  
  object1 = createBall(ball_radius, sc_locs[0][0], sc_locs[0][1], svs[0], b2Body.b2_dynamicBody, o_mass[0], damping, o_friction, o_elasticity, {
    name: "object1",
    bodyType: "dynamic",
    W: ball_radius,
    H: ball_radius
  });

  object2 = createBall(ball_radius, sc_locs[1][0], sc_locs[1][1], svs[1], b2Body.b2_dynamicBody, o_mass[1], damping, o_friction, o_elasticity, {
    name: "object2",
    bodyType: "dynamic",
    W: ball_radius,
    H: ball_radius
  });


  //(r, x, y, type, density, damping, friction, restitution, userData, isRendered, stage, img)
  object3 = createBall(ball_radius, sc_locs[2][0], sc_locs[2][1], svs[2], b2Body.b2_dynamicBody, o_mass[2], damping, o_friction, o_elasticity, {
    name: "object3",
    bodyType: "dynamic",
    W: ball_radius,
    H: ball_radius
  });

  object4 = createBall(ball_radius, sc_locs[2][0], sc_locs[2][1], svs[3], b2Body.b2_dynamicBody, o_mass[3], damping, o_friction, o_elasticity, {
    name: "object4",
    bodyType: "dynamic",
    W: ball_radius,
    H: ball_radius
  });

  objects = [object1, object2, object3, object4];


    idControlledObject = undefined;


  
    //Draw the fist anyway but make it size zero and inert
    //userFist = createBall(0, 0, 0, 0, b2Body.b2_staticBody, 0, ctrl_obj_damping, 0, 0, {
    //  name: "fist",
    //  bodyType: "dynamic",
    //  W: 0,
    //  H: 0
    // });


  world.SetContactListener(listener);

  ///////////////////////
  //Add all the forces
  ///////////////////////


  //A matrix of all local forces (using the lower triangle)
  var gravityControllers = [
    [undefined,
      new b2GravityController(),
      new b2GravityController(),
      new b2GravityController()
    ],
    [undefined,
      undefined,
      new b2GravityController(),
      new b2GravityController()
    ],
    [undefined,
      undefined,
      undefined,
      new b2GravityController()
    ],
    [undefined,
      undefined,
      undefined,
      undefined
    ]
  ];

  for (var i = 0; i < gravityControllers.length; i++) {
    for (var j = 0; j < gravityControllers.length; j++) {

      if (local_forces[i][j] != 0 & j > i) {
        gravityControllers[i][j].G = local_forces[i][j];
        //console.log('added attractive force between', i, j, 'of ', local_forces[i][j]);

        gravityControllers[i][j].AddBody(objects[i]);
        gravityControllers[i][j].AddBody(objects[j]);
        world.AddController(gravityControllers[i][j]);

      }
    }
  }




  var d = new Date();
  data.setup.start_time = d.getTime();
  data.setup.box2d_ratio = ratio;
  data.setup.pixel_ratio = 1//window.devicePixelRatio;
  data.setup.fullX = fullX;
  data.setup.fullY = fullY;
  data.setup.ctrl_obj_damping = ctrl_obj_damping;
  data.setup.box2d_step_size = stepSize;
  data.setup.local_forces = local_forces;
  data.setup.o_mass = o_mass;
  data.setup.o_radius = ball_radius;
  data.setup.o_elasticity = o_elasticity;
  data.setup.o_friction = o_friction;
  data.setup.o_damping = damping;
  data.setup.border_width = borderWidth;
  data.setup.wall_mass = wall_mass;
  data.setup.wall_elasticity = wall_elasticity;
  data.setup.wall_friction = wall_friction;
  data.setup.wall_damping = wall_damping;

  //  console.log('data.setup', data.setup);

     
  StartUpdate();
    
  //////////////////////
  //End of start function
  /////////////////////

}
////////////////////////////////////////////////
//Makes a square object in the box2d environment
////////////////////////////////////////////////
function createBox(w, h, x, y, type, density, damping, friction, restitution, userData) {

  // Create the fixture definition
  var fixDef = new b2FixtureDef;

  fixDef.density = density; // Set the density
  fixDef.friction = friction; // Set the friction 
  fixDef.restitution = restitution; // Set the restitution - elasticity

  // Define the shape of the fixture
  fixDef.shape = new b2PolygonShape;
  fixDef.shape.SetAsBox(
    w // input should be half the width
    , h // input should be half the height 
  );

  // Create the body definition
  var bodyDef = new b2BodyDef;
  bodyDef.type = type;

  // Set the position of the body
  bodyDef.position.x = x;
  bodyDef.position.y = y;


  // Create the body in the box2d world
  var b = world.CreateBody(bodyDef);
  b.CreateFixture(fixDef);

  //What is userData exactly, and how do we use it?
  if (typeof userData !== 'undefined') {
    b.SetUserData(userData);
  }

  b.m_linearDamping = damping;


  bodies.push(b);

  return b;
}



////////////////////////////////////////////////
//Makes a round object in the box2d environment
////////////////////////////////////////////////
function createBall(r, x, y, starting_vec, type, density, damping, friction, restitution, userData) {
  // Create the fixture definition
  var fixDef = new b2FixtureDef;

  fixDef.density = density; // Set the density
  fixDef.friction = friction; // Set the friction
  fixDef.restitution = restitution; // Set the restitution - bounciness

  // Define the shape of the fixture
  fixDef.shape = new b2CircleShape;
  fixDef.shape.SetRadius(r);

  // Create the body definition
  var bodyDef = new b2BodyDef;
  bodyDef.type = type;

  // Set the position of the body
  bodyDef.position.x = x;
  bodyDef.position.y = y;

  // Create the body in the box2d world
  var b = world.CreateBody(bodyDef);
  b.CreateFixture(fixDef);

  if (typeof userData !== 'undefined') {
    b.SetUserData(userData);
  }

  //this workaround seems to do the trick
  b.m_linearDamping = damping;

  b.SetLinearVelocity(starting_vec);
  //.ApplyImpulse(up, bodies[i].GetWorldCenter());

  //console.log('b', b.density, b.m_linearDamping);
  
  bodies.push(b);


  return b;
}



function getSensorContact(contact) {
  var fixtureA = contact.GetFixtureA();
  var fixtureB = contact.GetFixtureB();

  var sensorA = fixtureA.IsSensor();
  var sensorB = fixtureB.IsSensor();

  if (!(sensorA || sensorB))
    return false;

  var bodyA = fixtureA.GetBody();
  var bodyB = fixtureB.GetBody();

  if (sensorA) { // bodyB should be added/removed to the buoyancy controller
    return {
      sensor: bodyA,
      body: bodyB
    };
  } else { // bodyA should be added/removed to the buoyancy controller
    return {
      sensor: bodyB,
      body: bodyA
    };
  }
}


function onEF() {


  //Step the world forward
  world.Step(stepSize, 3, 3);
  world.ClearForces();
  
  ////////////////////
  //SAVE THE DATA HERE
  ////////////////////
  var tmp = [];
  
  tmp.push(data.timeline.length + 1); //Store row number
  
  for (var i = 0; i < bodies.length; i++) {
    
    var body = bodies[i];
    var p = body.GetPosition();
    //var checkrefresh = (element) => element == frameNum
    
    
    //Snap back bodies to timeline
    if ( Math.round(frameNum/snap_at)==frameNum/snap_at)
    {
     if (body.m_userData.name == 'object1') {
       //console.log('refreshing positions: ', frameNum);
       body.SetLinearVelocity(new b2Vec2(original_paths["o1.vx"][frameNum], original_paths["o1.vy"][frameNum]));
       body.SetPosition(new b2Vec2(original_paths["o1.x"][frameNum], original_paths["o1.y"][frameNum]));
     } else if (body.m_userData.name == 'object2') {
       body.SetLinearVelocity(new b2Vec2(original_paths["o2.vx"][frameNum], original_paths["o2.vy"][frameNum]));
       body.SetPosition(new b2Vec2(original_paths["o2.x"][frameNum], original_paths["o2.y"][frameNum]));
     } else if (body.m_userData.name == 'object3') {
       body.SetLinearVelocity(new b2Vec2(original_paths["o3.vx"][frameNum], original_paths["o3.vy"][frameNum]));
       body.SetPosition(new b2Vec2(original_paths["o3.x"][frameNum], original_paths["o3.y"][frameNum]));
     } else if (body.m_userData.name == 'object4') {
       body.SetLinearVelocity(new b2Vec2(original_paths["o4.vx"][frameNum], original_paths["o4.vy"][frameNum]));
       body.SetPosition(new b2Vec2(original_paths["o4.x"][frameNum], original_paths["o4.y"][frameNum]));
     }
     p = body.GetPosition();
    }
    
    if (body.m_userData.bodyType == "dynamic") {
      tmp.push(Math.round(p.x * 1000) / 1000);
      tmp.push(Math.round(p.y * 1000) / 1000);
      tmp.push(Math.round(body.m_linearVelocity.x * 1000) / 1000);
      tmp.push(Math.round(body.m_linearVelocity.y * 1000) / 1000);
    }
    
  }
  
  //Store that a snap-back occurred
  if ( Math.round(frameNum/snap_at)==(frameNum/snap_at) )
  {
    tmp.push('yes');
  } else {
    tmp.push('no');
  }
  data.timeline[data.timeline.length] = tmp; //Append the data

  //////////////////////////////
  //Update the controlled object
  //////////////////////////////

  var id_ctrl = original_paths['idco'][frameNum];

  
  if (id_ctrl != 'none') {
      xPos = original_paths.mouseX[frameNum];//e.target.mouseX;
      yPos = original_paths.mouseY[frameNum];
    //console.log(id_ctrl);
    
    for (var i = 0; i < bodies.length; i++)
    {
      //console.log(bodies[i].m_userData.name);
      if (bodies[i].m_userData.name == id_ctrl)
      {
        var cbody = bodies[i];
        bodies[i].m_linearDamping = ctrl_obj_damping;
      }  else {
        bodies[i].m_linearDamping = damping;
      }
    }
    
    var xCO = cbody.GetPosition().x; //Position of controlled object
    var yCO = cbody.GetPosition().y; //Position of controlled object

    //This is pretty heuristic force increases rapidly for farther distances from cursor
    //but is also damped by the current velocity to prevent it getting too crazy
    var xVec = .2*Math.pow((xPos / ratio - xCO), 1); //fistSpeed; 1
    var yVec = .2*Math.pow((yPos / ratio - yCO), 1); //fistSpeed;    1
    var armForce = new b2Vec2(xVec, yVec);
    cbody.ApplyImpulse(armForce, cbody.GetWorldCenter())
    
    //if (frameNum<400){
    //  console.log('frame: ', frameNum, ' id_ctrl: ', id_ctrl, ' x: ', xPos / ratio, ' x_co: ', xCO, ' diff: ', xPos / ratio - xCO);
    //}

    //console.log(id_ctrl);
  }

}



function StartUpdate() {
  
  //console.log('Started the onEF process');
  
  for (frameNum = 0; frameNum<stopTime; frameNum++)
  {
    onEF();
  }
  
  //End of simulation stuff....
  for (var i = 0; i < bodies.length; i++) {
    var body = bodies[i];
    //And destroy bodies?
    world.DestroyBody(body);
  }
  bodies = []; // clear the body and actor vectors;
}


var listener = new b2ContactListener();
listener.BeginContact = function(contact) {

  //Patch stuff??
  var contactEntities = getSensorContact(contact);

  if (contactEntities) {
    console.log('inner', contactEntities.sensor, contactEntities.sensor.GetUserData())
    var sensor = contactEntities.sensor;
    if (sensor.GetUserData()) {
      var userData = sensor.GetUserData();
      if (userData.controller) {
        console.log('inner inner', userData.controller)
        userData.controller.AddBody(contactEntities.body);

      }
    }
  }

}



listener.EndContact = function(contact) {
  var contactEntities = getSensorContact(contact);

  if (contactEntities) {
    var sensor = contactEntities.sensor;
    if (sensor.GetUserData()) {
      var userData = sensor.GetUserData();
      if (userData.controller) {
        userData.controller.RemoveBody(contactEntities.body);
      }
    }
  }
}


function RandomLocations(n) {

  var array = [];

  array.push([Math.random() * fullX, Math.random() * fullY]);
  console.log('new locations...')
    //Loop over the number of new locations needed

  for (var i = 1; i < n; i++) {

    var okLoc = false;
    var timeout = 0;
    while (okLoc == false & timeout < 250) {
      timeout = timeout + 1;

      proposal = [Math.random() * fullX, Math.random() * fullY];
      //Check loc
      okLoc = true;

      for (var j = 0; j < array.length; j++) {
        //Check they don't overlap
        if ((proposal[0] - array[j][0]) < ball_radius &
          (proposal[0] - array[j][0]) > (-ball_radius) &
          (proposal[1] - array[j][1]) < ball_radius &
          (proposal[1] - array[j][1]) > (-ball_radius)) {
          //console.log('conflict', array[j], proposal)
          okLoc = false;
        }
        //Check they are not within a ball width of the edge
        if (proposal[0] < (2 * ball_radius) | proposal[0] > (fullX - (2 * ball_radius)) |
          proposal[1] < (2 * ball_radius) | proposal[1] > (fullY - (2 * ball_radius))) {
          //console.log('Too near edge', proposal)
          okLoc = false;
        }
      }

    }
    //console.log('no conflict', array, proposal)
    array.push(proposal)
  }

  return array;
}

Start(cond);



//console.log(data, data.setup);
