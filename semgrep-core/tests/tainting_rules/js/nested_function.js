var app = angular.module('MyApp', []);
app.controller('myCtrl', function($scope, $sce) {
    $scope.sayHello = function() {
     value = $scope.html
     //ERROR:
     $sce.trustAs($sce.HTML, value);
   };
});
