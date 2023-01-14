function($scope, $sce) {
   value = $scope.html
   //ERROR:
   $sce.trustAs($sce.HTML, value);
}
