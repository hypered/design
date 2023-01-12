import {
  TitleJumbo,
  SubTitleJumbo,
  Title,
  SubTitle,
  P,
  Divider,
} from "../../components";

export default {
  title: "Title",
};

export const TitleJumboExample = () => (
  <TitleJumbo>
    Hypered <span className="normal">design system</span>
  </TitleJumbo>
);

export const SubtitleJumboExample = () => (
  <SubTitleJumbo>Introduction</SubTitleJumbo>
);

export const TitleExample = () => (
  <Title>Introduction</Title>
);

export const SubtitleExample = () => (
  <SubTitle>Introduction</SubTitle>
);

export const JumboUsage = () => (
  <div className="mw8 center">
    <header className="pv4 pv5-l">
      <TitleJumbo>Hypered</TitleJumbo>
      <SubTitleJumbo>Software development, defined</SubTitleJumbo>
      <P>
        Lorem ipsum dolor sit amet, consectetur adipiscing elit. Praesent
        convallis mollis nulla, molestie tempor velit consequat non.
      </P>
    </header>
    <Divider />
    <section className="pv4 pv5-l">
      <SubTitleJumbo>Introduction</SubTitleJumbo>
      <P>
        Lorem ipsum dolor sit amet, consectetur adipiscing elit. Praesent
        convallis mollis nulla, molestie tempor velit consequat non. Integer
        quam ligula, consequat eget semper in, sodales nec mauris. Sed ultrices
        enim quis eros lobortis, semper condimentum eros sodales. Morbi iaculis
        lectus id dui convallis feugiat.
      </P>
    </section>

    <Divider />

    <section className="pv4 pv5-l">
      <div className="flex flex-wrap nl3 nr3 tc">
        <div className="w-100 w-third-l ph3">
          <SubTitleJumbo>23,000</SubTitleJumbo>
          <P>downloads</P>
        </div>

        <div className="w-100 w-third-l ph3">
          <SubTitleJumbo>3.2kb</SubTitleJumbo>
          <P>gzipped</P>
        </div>

        <div className="w-100 w-third-l ph3">
          <SubTitleJumbo>626</SubTitleJumbo>
          <P>stars on GitHub</P>
        </div>
      </div>
    </section>
  </div>
);

export const Usage = () => (
  <div className="mw8 center">
    <header className="pv4 pv5-l">
      <Title>Hypered</Title>
      <SubTitle>Software development, defined</SubTitle>
      <P>
        Lorem ipsum dolor sit amet, consectetur adipiscing elit. Praesent
        convallis mollis nulla, molestie tempor velit consequat non.
      </P>
    </header>
    <Divider />
    <section className="pv4 pv5-l">
      <SubTitle>Introduction</SubTitle>
      <P>
        Lorem ipsum dolor sit amet, consectetur adipiscing elit. Praesent
        convallis mollis nulla, molestie tempor velit consequat non. Integer
        quam ligula, consequat eget semper in, sodales nec mauris. Sed ultrices
        enim quis eros lobortis, semper condimentum eros sodales. Morbi iaculis
        lectus id dui convallis feugiat.
      </P>
    </section>

    <Divider />

    <section className="pv4 pv5-l">
      <div className="flex flex-wrap nl3 nr3 tc">
        <div className="w-100 w-third-l ph3">
          <SubTitle>23,000</SubTitle>
          <P>downloads</P>
        </div>

        <div className="w-100 w-third-l ph3">
          <SubTitle>3.2kb</SubTitle>
          <P>gzipped</P>
        </div>

        <div className="w-100 w-third-l ph3">
          <SubTitle>626</SubTitle>
          <P>stars on GitHub</P>
        </div>
      </div>
    </section>
  </div>
);
