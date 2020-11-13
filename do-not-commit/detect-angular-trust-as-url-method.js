var app = angular.module('MyApp', []);
app.controller('myCtrl', function($scope, $sce) {

$scope.userInput = 'foo';
    $scope.sayHello = function() {
     // ruleid:detect-angular-trust-as-url-method
     $scope.trustedurl = $sce.trustAsUrl($scope.html);
     // ruleid:detect-angular-trust-as-url-method
     input = $scope.html
     $scope.trustedurl = $sce.trustAsUrl(input);


     //Data is not coming from user input
     $scope.trustedurl = $sce.trustAsUrl('stringLiteral');
   };

});
