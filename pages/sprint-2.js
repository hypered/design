import React from "react";

import {
  Layout,
  Input,
  Button,
  ButtonDisabled,
  Table,
  TH1,
  TH2,
  TD1,
  TD2,
  AlertGreen,
  AlertRed,
  AlertYellow,
} from "../components";

import data from "../data/bus-stops";

function Sprint2() {
  return (
    <Layout>
      <h1>Sprint 2</h1>
      <p></p>

      <section className="pb5">
        <h3>Table—variant 1</h3>
        <div className="mw7 pb4">
          <Table
            th={
              <tr>
                <TH1>routeId</TH1>
                <TH1>routeNm</TH1>
                <TH1>stopId</TH1>
                <TH1>stopNm</TH1>
                <TH1>stopSeq</TH1>
                <TH1>stopCode</TH1>
              </tr>
            }
            td={data.slice(0, 10).map(busStop => {
              return (
                <tr>
                  <TD1>{busStop.routeId}</TD1>
                  <TD1>{busStop.routeNm}</TD1>
                  <TD1>{busStop.stopId}</TD1>
                  <TD1>{busStop.stopNm}</TD1>
                  <TD1>{busStop.stopSeq}</TD1>
                  <TD1>{busStop.stopCode}</TD1>
                </tr>
              );
            })}
          ></Table>
        </div>

        <h3>Table—variant 2</h3>
        <div className="w-100">
          <Table
            th={
              <tr>
                <TH2>routeId</TH2>
                <TH2>routeNm</TH2>
                <TH2>lineId</TH2>
                <TH2>lineNm</TH2>
                <TH2>stopId</TH2>
                <TH2>stopNm</TH2>
                <TH2>
                  <div className="tr">stopSeq</div>
                </TH2>
                <TH2>
                  <div className="tr">stopCode</div>
                </TH2>
              </tr>
            }
            td={data.slice(0, 50).map(busStop => {
              return (
                <tr>
                  <TD2>{busStop.routeId}</TD2>
                  <TD2>{busStop.routeNm}</TD2>
                  <TD2>{busStop.lingId}</TD2>
                  <TD2>{busStop.lingNm}</TD2>
                  <TD2>{busStop.stopId}</TD2>
                  <TD2>{busStop.stopNm}</TD2>
                  <TD2>
                    <div className="tr">{busStop.stopSeq}</div>
                  </TD2>
                  <TD2>
                    <div className="tr">{busStop.stopCode}</div>
                  </TD2>
                </tr>
              );
            })}
          ></Table>
        </div>
      </section>

      <section>
        <h3>Input</h3>
        <div className="mw5">
          <Input type="text" label="Full Name" placeholder="John Doe" />
        </div>

        <h3>Button</h3>
        <div className="flex">
          <div className="mr3">
            <Button>Submit</Button>
          </div>
          <div className="mr3">
            <ButtonDisabled>Submit</ButtonDisabled>
          </div>
        </div>

        <h3>Login Form</h3>
        <div className="mw6">
          <form className="pa4 bg-white ba b--black bw1">
            <h2>Log in to your account</h2>
            <Input
              type="email"
              label="Email"
              placeholder="john@doe.com"
              message="You have entered an invalid email"
            />
            <Input type="password" label="Password" placeholder="" />
            <div className="flex flex-wrap justify-between">
              <a
                className="link no-underline black hover-blue self-center"
                href="#"
              >
                Forgot Password
              </a>
              <Button>Log In —></Button>
            </div>
          </form>
        </div>
      </section>

      <section>
        <h3>Alert Dialogs</h3>

        <div className="mw6">
          <AlertGreen>You have successfully logged in.</AlertGreen>
          <AlertRed>
            Uh-oh. You have entered the wrong email / password.
          </AlertRed>
          <AlertYellow>Your network seems to be down.</AlertYellow>
        </div>
      </section>
    </Layout>
  );
}

export default Sprint2;
