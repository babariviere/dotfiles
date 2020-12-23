const render = ({ output }) => {
  if (typeof output === "undefined") return null;
  const status = output.status;
  const ssid = output.ssid;
  if (status === "inactive") return <div>􀙈</div>;
  return <div>􀙇 {output.ssid}</div>;
};

export default render;
