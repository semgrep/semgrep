var app = angular.module('MyApp', []);
app.controller('myCtrl', function($scope, $sce) {

$scope.userInput = 'foo';
    $scope.sayHello = function() {
     // ruleid:detect-angular-trust-as-js-method
     $scope.trustedurl = $sce.trustAsJs($scope.html);
     // ruleid:detect-angular-trust-as-js-method
     input = $scope.html
     $scope.trustedurl = $sce.trustAsJs(input);


     //Data is not coming from user input
     $scope.trustedurl = $sce.trustAsJs('stringLiteral');
   };

});
