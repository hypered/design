import React from "react";
import App from "next/app";
import { createGlobalStyle } from "styled-components";
import Head from "next/head";
import { normalize } from "polished";

const Normalize = createGlobalStyle`${normalize()}`;

const GlobalStyle = createGlobalStyle`
  * {
    box-sizing: border-box;
  }

  body {
    background: #ffffff;
    color: #000000;
  	-webkit-font-smoothing: antialiased;
  	-moz-osx-font-smoothing: grayscale;
  }
`;

const InterFontCSS = createGlobalStyle`
  @import url('https://rsms.me/inter/inter.css');
  html {
    font-family: 'Inter', -apple-system, BlinkMacSystemFont, sans-serif;
  }
  @supports (font-variation-settings: normal) {
    html {
      font-family: 'Inter var', -apple-system, BlinkMacSystemFont, sans-serif;
    }
  }
`;

class MyApp extends App {
  static async getInitialProps({ Component, ctx }) {
    let pageProps = {};

    if (Component.getInitialProps) {
      pageProps = await Component.getInitialProps(ctx);
    }

    return { pageProps };
  }

  render() {
    const { Component, pageProps } = this.props;

    return (
      <>
        <Head>
          <link rel="stylesheet" href="https://rsms.me/inter/inter.css" />
          <link
            rel="stylesheet"
            href="https://unpkg.com/tachyons@4.10.0/css/tachyons.min.css"
          />
        </Head>

        <Normalize />
        <GlobalStyle />
        <InterFontCSS />
        <Component {...pageProps} />
      </>
    );
  }
}

export default MyApp;
