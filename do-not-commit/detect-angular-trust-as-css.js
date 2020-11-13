var app = angular.module('MyApp', []);
app.controller('myCtrl', function($scope, $sce) {

$scope.userInput = 'foo';
    $scope.sayHello = function() {
     // ruleid:detect-angular-trust-as-css-method
     $scope.trustedurl = $sce.trustAsCss($scope.html);
     // ruleid:detect-angular-trust-as-css-method
     input = $scope.html
     $scope.trustedurl = $sce.trustAsCss(input);


     //Data is not coming from user input
     $scope.trustedurl = $sce.trustAsCss('stringLiteral');
   };

});
