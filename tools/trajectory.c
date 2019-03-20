#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <stdint.h>

#include <glm/glm.hpp>
#include <glm/gtc/matrix_transform.hpp>

#define WIDTH 40
#define HEIGHT 25
#define N 64

int main() {    
    float p[6] = { M_PI, M_PI, M_PI/2.f,-M_PI/2.f, M_PI,     M_PI    };
    float t[6] = { M_PI, 0.0f, M_PI,    -M_PI/2.f, M_PI/2.f,-M_PI/2.f};

//    glm::vec3 eye(2.2f, 0.0f, 3.2f);
    glm::vec3 eye(1.2f, -2.4f, 2.4f);
    glm::vec3 center(0.f, 0.f, 0.f);
    glm::vec3 up(0.f, 1.f, 0.f);
    
    glm::mat4 cam = glm::lookAt(eye, center, up);
    glm::mat4 proj = glm::perspective(80.f*float(M_PI)/180.f, float(WIDTH)/float(HEIGHT), 0.5f, 16.f);

    uint8_t x[6][N];
    uint8_t y[6][N];

    for(int i=0; i<N; i++)
    {
        float beta = i * 2.f * M_PI / (float)N;

        float r     = 1.8f + 0.5f * sin(0.5f * M_PI + 2.f * beta);
        float theta = 1.f * beta;
        float phi   = 0.2f * M_PI * sin(2.f * beta);

        glm::vec3 o(r*cos(phi)*cos(theta), r*cos(phi)*sin(theta), r*sin(phi));

        int j;        
        for(j=0; j<6; j++) {
            glm::vec3 v(cos(phi+p[j]) * cos(theta+t[j]),
                        cos(phi+p[j]) * sin(theta+t[j]),
                        sin(phi+p[j]));
            //v += o;
            v *= r;
            
            glm::vec4 screen = proj * cam * glm::vec4(v.x, v.y, v.z, 1.f);
            int px = (WIDTH-16) * (1.f + screen.x / screen.w) / 2.f;
            int py = (HEIGHT-8) * (1.f + screen.y / screen.w) / 2.f;
            
            x[j][i] = px;
            y[j][i] = py;
        }
    }

    for(int j=0; j<6; j++) {
        printf("x_obj.%d:\n", j);
        for(int i=0; i<N; ) {
            printf("    defb ");
            for(int k=0; (k<16) && (i<N); k++, i++) {
                printf("0x%02x", x[j][i]);
                if(k<15) { printf(","); }
            }
            printf("\n");
        }
    }
    for(int j=0; j<6; j++) {
        printf("y_obj.%d:\n", j);
        for(int i=0; i<N; ) {
            printf("    defb ");
            for(int k=0; (k<16) && (i<N); k++, i++) {
                printf("0x%02x", y[j][i]);
                if(k<15) { printf(","); }
            }
            printf("\n");
        }
    }

    return EXIT_SUCCESS;
}