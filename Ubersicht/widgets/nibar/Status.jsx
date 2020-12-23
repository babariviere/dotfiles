import DateTime from "./lib/DateTime.jsx";
import Battery from "./lib/Battery.jsx";
import Error from "./lib/Error.jsx";
import parse from "./lib/parse.jsx";
import styles from "./lib/styles.jsx";


const style = {
  background: styles.colors.bg,
  borderRadius: 30,
  color: styles.colors.fg,
  display: "grid",
  fontFamily: styles.fontFamily,
  fontSize: styles.fontSize,
  fontWeight: styles.fontWeight,
  gridAutoFlow: "column",
  gridGap: "15px",
  lineHeight: styles.lineHeight,
  marginBottom: "5px",
  marginTop: "5px",
  overflow: "hidden",
  paddingBottom: "5px",
  paddingRight: "15px",
  paddingTop: "5px",
  position: "fixed",
  right: "10px",
};

const style2 = {
  fontSize: "18px",
  color: styles.colors.dim,
};

export const refreshFrequency = 10000;

export const command = "./nibar/scripts/status.sh";

export const render = ({ output }) => {
  const data = parse(output);
  if (typeof data === "undefined") {
    return (
      <div style={style}>
        <Error msg="Error: unknown script output" side="right" />
      </div>
    );
  }
  return (
    <div style={style}>
      <span style={style2}></span>
      <Battery output={data.battery} />
      <DateTime output={data.datetime} />
    </div>
  );
};

export default null;
