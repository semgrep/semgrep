// bad; TODO, how do I match this?
class App extends React.Component {
  constructor(props) {
    super(props);
    //ERROR: match
    try {
      const savedOrgData = localStorage.getItem(ORGANIZATION_KEY);
      if (savedOrgData) {
        this.state = {
          org: JSON.parse(savedOrgData),
        };
      } else {
        this.state = {};
      }
    } catch (e) {
      const savedOrgData = localStorage.getItem(ORGANIZATION_KEY);
      console.warn(
        "Unable to save org data to localStorage. Likely due to a browser setting. You might see some weird behavior",
        e
      );
      this.state = {};
    }
  }
}
