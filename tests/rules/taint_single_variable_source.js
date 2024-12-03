function($scope, $sce) {
   value = $scope.html;
   // ruleid: test
   $sce.sink(value);
}
