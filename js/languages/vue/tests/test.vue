<template>
  <p>
    Hello, <a :[key]="url">{{ name }}</a
    >!
  </p>
</template>

<script>
//src: https://ikatyang.github.io/tree-sitter-vue/
module.exports = {
  data: function () {
    return {
      name: "World",
      key: "href",
      url: "https://example.com/",
    };
  },
};
</script>

<style scoped>
p {
  font-size: 2em;
  text-align: center;
}
</style>
