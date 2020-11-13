var app = angular.module('MyApp', []);
app.controller('myCtrl', function($scope, $sce) {

$scope.userInput = 'foo';
    $scope.sayHello = function() {
     // ruleid:detect-angular-trust-as-resourceurl-method
     $scope.trustedurl = $sce.trustAsResourceUrl($scope.html);
     // ruleid:detect-angular-trust-as-resourceurl-method
     input = $scope.html
     $scope.trustedurl = $sce.trustAsResourceUrl(input);


     //Data is not coming from user input
     $scope.trustedurl = $sce.trustAsResourceUrl('stringLiteral');
   };

});
