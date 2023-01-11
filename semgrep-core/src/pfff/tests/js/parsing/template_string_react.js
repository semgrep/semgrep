import React, { Component } from 'react'
import styled, { keyframes } from 'styled-components'
import logo from './logo.svg'

const AppWrapper = styled.div`
  text-align: center;
`

const rotate360 = keyframes`
  from {
    transform: rotate(0deg);
  }
  to {
    transform: rotate(360deg);
  }
`

const AppLogo = styled.img`
  animation: ${rotate360} infinite 120s linear;
  height: 80px;
  &:hover {
    animation: ${rotate360} infinite 1.5s linear;
  }
`

const AppHeader = styled.div`
  background-color: #222;
  height: 12rem;
  padding: 1rem;
  color: white;
`

const AppTitle = styled.h1`
  font-weight: 900;
`

const AppIntro = styled.p`
  font-size: large;
  code {
    font-size: 1.3rem;
  }
`

const EmojiWrapper = styled.span.attrs({
  role: 'img'
})``

class App extends Component {
  render() {
    return (
      <AppWrapper>
        <AppHeader>
          <AppLogo src={logo} alt="logo" />
          <AppTitle>Welcome to React</AppTitle>
        </AppHeader>
        <AppIntro>
          Bootstrapped with <code>create-react-app</code>.
        </AppIntro>
        <AppIntro>
          Components styled with <code>styled-components</code>{' '}
          <EmojiWrapper aria-label="nail polish" />
        </AppIntro>
      </AppWrapper>
    )
  }
}

export default App
