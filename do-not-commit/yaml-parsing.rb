# cf. https://github.com/presidentbeef/brakeman/blob/98d50e94921335d31cba811c712e8840ee197d8b/test/apps/rails_with_xss_plugin/config/initializers/yaml_parsing.rb

# ruleid:yaml-parsing
ActionController::Base.param_parsers[Mime::YAML] = :yaml

# ok
ActiveSupport::CoreExtensions::Hash::Conversions::XML_PARSING.delete('symbol')
# ok
ActiveSupport::CoreExtensions::Hash::Conversions::XML_PARSING.delete('yaml')
