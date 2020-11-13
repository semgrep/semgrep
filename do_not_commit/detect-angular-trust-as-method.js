var app = angular.module('MyApp', []);
app.controller('myCtrl', function($scope, $sce) {

$scope.userInput = 'foo';
    $scope.sayHello = function() {
     
     // ruleid:detect-angular-trust-as-method
     value = $scope.html 
     // ruleid:detect-angular-trust-as-method
     $sce.trustAs($sce.HTML, value);
     // ruleid:detect-angular-trust-as-method
     $sce.trustAs($sce.CSS, value);     
     // ruleid:detect-angular-trust-as-method
     $sce.trustAs($sce.JS, value);
     // ruleid:detect-angular-trust-as-method
     $sce.trustAs($sce.RESOURCE_URL, value);
     // ruleid:detect-angular-trust-as-method
     $sce.trustAs($sce.URL, value);
   

     //Data is not coming from user input
     $scope.trustedurl = $sce.trustAs('stringLiteral');
   };

});
