#version 110

uniform sampler2D tex;
uniform vec3 col;

varying vec2 texcoord;

void main()
{
  gl_FragColor = texture2D(tex, texcoord);
  
  //gl_FragColor = vec4(col, 1.0);

  // gl_FragColor = mix(
  //       texture2D(textures[0], texcoord),
  //       texture2D(textures[1], texcoord),
  //       0.5);
}

// uniform vec3 col;

// void main(void) {
//   gl_FragColor = vec4(col, 1.0);
// }

