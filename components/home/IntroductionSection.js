import { Section, H1, P } from "../../components";

export const IntroductionSection = props => (
  <Section>
    <H1>Introduction</H1>
    <pre className="bg-light-gray pa3">
      <code>Hello</code>
    </pre>
    {/* @TODO: replace copy */}
    <P>
      Lorem ipsum dolor sit amet, consectetur adipiscing elit. Praesent
      convallis mollis nulla, molestie tempor velit consequat non. Integer quam
      ligula, consequat eget semper in, sodales nec mauris. Sed ultrices enim
      quis eros lobortis, semper condimentum eros sodales. Morbi iaculis lectus
      id dui convallis feugiat.
    </P>
    <pre className="bg-light-gray pa3">
      <code>Hello</code>
    </pre>
  </Section>
);
