#version 110

uniform sampler2D texture0;
varying vec2 texcoord;

void main()
{
  gl_FragColor = texture2D(texture0, texcoord);
}

// uniform vec3 col;

// void main(void) {
//   gl_FragColor = vec4(col, 1.0);
// }

