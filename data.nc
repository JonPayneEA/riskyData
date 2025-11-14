


<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="utf-8">
    <title>Login</title>
    <meta name="viewport" content="width=device-width, initial-scale=1">
    <meta http-equiv="X-UA-Compatible" content="IE=edge" />
    
    
    
    
        <!-- Bootstrap Core
        CSS -->
        <link rel="stylesheet" href="https://artefacts.ceda.ac.uk/themes/orgtheme_ceda_serv/0.2/4/flatly/bootstrap.css" media="screen">
        <!-- Custom CSS -->
        <link rel="stylesheet" href="https://artefacts.ceda.ac.uk/themes/orgtheme_ceda_serv/0.2/_assets/css/custom.min.css">
        <link rel="stylesheet" href="https://artefacts.ceda.ac.uk/themes/orgtheme_ceda_serv/0.2/_assets/css/org-custom.css">

        
        

        <link href="/static/fwtheme_django/css/fw-custom.css" rel="stylesheet">
        <!-- Custom Fonts -->
        <link rel="stylesheet" href="https://use.fontawesome.com/releases/v5.0.10/css/all.css" integrity="sha384-+d0P83n9kaQMCwj8F4RJB66tzIwOKmrdb46+porD/OvrJ+37WqIM7UoBtwHO6Nlg" crossorigin="anonymous">

        
        
<link href="/static/dj_security/css/dj_security_custom.css" rel="stylesheet">


    

    
      <script src="/static/cookielaw/js/cookielaw.js"></script>
    

    

  </head>
  <body id="body">
  
  
      
            <div class="alert alert-info text-center" id="CookielawBanner">
            This website uses cookies.
            By continuing to use this website you are agreeing to our use of cookies.&nbsp;
                <div class="btn-group">
                    <button type="button" class="btn btn-secondary"
                            onclick="javascript:Cookielaw.createCookielawCookie();">OK</a>

                    <button type="button" class="btn btn-secondary"
                            onclick="javascript:window.open('http://www.ceda.ac.uk/privacy-and-cookies/')">Find out more</button>
                </div>
            </div>
      
  
 
    
    <div class="navbar navbar-expand-lg fixed-top navbar-dark bg-primary">
        <div class="container">
            
    <a href="http://archive.ceda.ac.uk/" class="navbar-brand">
        <img src="https://artefacts.ceda.ac.uk/themes/orgtheme_ceda_serv/0.2/_assets/img/ceda_archive_logo_transp_white_3_h80.png" alt="CEDA logo">
    </a>
    
            <button class="navbar-toggler" type="button" data-toggle="collapse" data-target="#navbarResponsive" aria-controls="navbarResponsive" aria-expanded="false" aria-label="Toggle navigation">
                <span class="navbar-toggler-icon"></span>
            </button>
            
                
            
            
            
            
        </div>
    </div>
    

    
    

    
    

    

    <div class="container">

      
      

      <div class="page-header" id="banner">
        <div class="row">
          <div class="col-lg-8 col-md-7 col-sm-6">
            
<div class="">
    <h1>Login</h1>
</div>

          </div>
          
          <div class="col-lg-4 col-md-5 col-sm-6">
            <div class="sponsor">
                  
              
            </div>
          </div>
          
        </div>
      </div>

      

      
      <div class="row">
          <div class="col-md-12">
              
<div class="d-md-flex">
    <div class="col-lg-12 col-md-12 col-sm-12 col-xs-6 text-center">

        <div class="row">

            <div id="login" class="col-lg-7 col-md-7 col-sm-12 col-xs-6 mt-4">
                <div class="row justify-content-left">
                    <div class="col-lg-12">
                    
                        
                        
                        <form class="form-signin" id="auth_form" name="auth_form" action="/account/signin/" method="post">
                            <input type="hidden" name="csrfmiddlewaretoken" value="ssIqApy9OJ3dr65hjO13mfeivYXIdjPeC58AreW9eQjszJTh3SDIcKgOica0ml75">
                            
                            <div class="form-row mb-2">
                                <label for="id_username" class="sr-only">Username</label>
                                <input type="text" name="username" id="id_username" class="form-control" maxlength="40" placeholder="Username" required autofocus>
                            </div>
                            
                            <div class="form-row mb-2">
                                <label for="id_password" class="sr-only">Password</label>
                                <input type="password" name="password" id="id_password" class="form-control" placeholder="Password" required>
                            </div>
                            
                            <div class="form-row">
                                <button class="btn btn-lg btn-primary btn-block" type="submit">Sign in</button>
                            </div>
                            
                        </form>
                    </div>
                </div>
            </div>

            <div class="col-lg-5 col-md-5 col-sm-12 col-xs-6 mt-4">
                <div class="row justify-content-center">
                    <div class="col-lg-10">
                        <p>
                            <a href="https://accounts.ceda.ac.uk/realms/ceda/account/#/personal-info" class="btn btn-lg btn-primary btn-block" >Register for an account!</a>
                        </p>
                        <p class="justify-content-around border border-light border-bottom-0 border-left-0 border-right-0 lead pt-2">
                            <a href="https://accounts.ceda.ac.uk/realms/ceda/account/#/personal-info">Forgot your password?</a>
                        </p>
                        <p class="justify-content-around border border-light border-bottom-0 border-left-0 border-right-0 lead pt-2">
                            Problems logging on? Contact <a href="http://www.ceda.ac.uk/contact">CEDA</a> support for help
                        </p>
                    </div>
                </div>
            </div>

        </div>
    </div>
</div>


          </div>
      </div>
      

      


    </div>
    

    
    <footer>
      <div class="container">
          <div class="row d-flex justify-content-around border border-light border-bottom-0 border-left-0 border-right-0 pt-5 pb-2 mt-5">
            <div class="col col-md-4 text-center ">
                <a href="http://www.ncas.ac.uk" title="NCAS"><img src="https://artefacts.ceda.ac.uk/themes/orgtheme_ceda_serv/0.2/_assets/img/ncas_logo_transparent_blacktext.png" title="NCAS logo" alt="NCAS logo" ></a>
            </div>
            <div class="col-md-4">
                <p class="text-center small">
                    Copyright © 2018 <a href="http://www.stfc.ac.uk">STFC</a>
                    All Rights Reserved<br>
                    <a href="http://www.ceda.ac.uk/accessibility" target="_blank">Accessibility</a> |
                    <a href="http://www.ceda.ac.uk/disclaimer" target="_blank">Disclaimer</a> |
                    <a href="http://www.ceda.ac.uk/privacy-and-cookies/" target="_blank">Privacy and Cookies</a><br/>
                    <a href="http://www.ceda.ac.uk" target="_blank">CEDA</a>
                </p>
            </div>
            <div class="col col-md-4 text-center">
                <a href="http://www.nceo.ac.uk" title="STFC"><img src="https://artefacts.ceda.ac.uk/themes/orgtheme_ceda_serv/0.2/_assets/img/nceologo200.png" title="NCEO logo" alt="NCEO logo"></a>
            </div>
          </div>
        </div>
      </footer>
      


    <script src="https://artefacts.ceda.ac.uk/themes/orgtheme/0.2/_vendor/jquery/dist/jquery.min.js"></script>
    <script src="https://artefacts.ceda.ac.uk/themes/orgtheme/0.2/_vendor/popper.js/dist/umd/popper.min.js"></script>
    <script src="https://artefacts.ceda.ac.uk/themes/orgtheme/0.2/_vendor/bootstrap/dist/js/bootstrap.min.js"></script>
    <script src="https://artefacts.ceda.ac.uk/themes/orgtheme/0.2/_assets/js/custom.js"></script>
    
    
  </body>
</html>
