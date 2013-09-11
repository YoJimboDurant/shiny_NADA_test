function(){
  tabPanel("About this Application",
           p(style="text-align:justify",'This online application was developed to simulate substitution methods for left-censored data as compared to NADA'),
           p(style="text-align:justify",'However, because the application has been developed using the ',a("Shiny", href="http://www.rstudio.com/shiny/", target="_blank"),'package and ',a("R software", href="http://r-project.org/", target="_blank"),' 
             the mathematical extensibility is virtually unlimited. We hope that you find this online tool useful for your purposes ! This is free and unencumbered software released into the public domain.

            Anyone is free to copy, modify, publish, use, compile, sell, or
            distribute this software, either in source code form or as a compiled
            binary, for any purpose, commercial or non-commercial, and by any
            means.
            
            In jurisdictions that recognize copyright laws, the author or authors
            of this software dedicate any and all copyright interest in the
            software to the public domain. We make this dedication for the benefit
            of the public at large and to the detriment of our heirs and
            successors. We intend this dedication to be an overt act of
            relinquishment in perpetuity of all present and future rights to this
            software under copyright law.
            
            THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
            EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
            MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
            IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR
            OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
            ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
            OTHER DEALINGS IN THE SOFTWARE.
            
            For more information, please refer to <http://unlicense.org/>'),
           br(),
           
           HTML('<div style="clear: left;"><img src="Hubona-small-jpeg.jpg" alt="" style="float: left; margin-right:5px" /></div>'),
           strong('Author and Application Developer'),
           p('James T Durant',br(),
             'Industrial Hygienist | R Enthusiast',br(),
             a('Check out my blog', href="http://jamestdurant.wordpress.com/", target="_blank"),
             '|',
             a('Some other thing', href="http://www.google.com", target="_blank")
           ),
           br(),
           
           div(class="row-fluid",
               # div(class="span3",strong('Other app versions'),
               #      p(HTML('<ul>'),
               #       HTML('<li>'),a("Version 1", href="XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX", target="_blank"),HTML('</li>'),
               #        HTML('<li>'),a("Version 2", href="XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX", target="_blank"),HTML('</li>'),
               #        HTML('<li>'),a("Version 3", href="XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX", target="_blank"),HTML('</li>'),
               #        HTML('</ul>')),
               #      strong('Code'),
               #       p('Source code available at',
               #         a('GitHub', href='https://github.com/ua-snap/shiny-apps/tree/master/RVdistsExampleAppV4')),
               #       br()
               #   ),
               
               div(class="span3", strong('NADA Principals'),
                   p(HTML('<ul>'),
                     HTML('<li>'),a("Person A", href="http:www.google.com", target="_blank"),HTML('</li>'),
                     HTML('</ul>')),
                   br()
               ),
               
               div(class="span3", strong('Educational Resources'),
                   p(HTML('<ul>'),
                     HTML('<li>'),a("Resource A", href="http:www.google.com", target="_blank"),HTML('</li>'),
                     HTML('</ul>')),
                   br()
               ),
               
               div(class="span3",
                   strong('Reference Documentation'),
                   p(HTML('<ul>'),
                     HTML('<li>'),a('Documentation A', href="http://www.google.com", target="_blank"),HTML('</li>'),
                     HTML('<li>'),a("shiny package in R", href="http://www.rstudio.com/shiny/", target="_blank"),HTML('</li>'),
                     HTML('<li>'),a('The R Project', href="http://www.r-project.org/", target="_blank"),HTML('</li>'),
                     HTML('</ul>'))
               )
           )
           )
}