var app = angular.module('MyApp', []);
app.controller('myCtrl', function($scope, $sce) {
    $scope.sayHello = function() {
     value = $scope.html
     //ruleid: detect-angular-trust-as-method
     $sce.trustAs($sce.HTML, value);
   };
});
