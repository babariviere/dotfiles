import { styled, run } from "uebersicht";
import getColors from "./lib/getColors.js";
const _version = "1.2.4";

/* CUSTOMIZATION (mess around here!)
You may need to refresh the widget after changing these settings
For more details about these settings: please visit https://github.com/aCluelessDanny/UeberPlayer#settings */

const options = {
  /* Widget position! */
  verticalPosition: "5px", // -> top (default) | center | bottom | "<number>" | "-<number>"
  horizontalPosition: "10px", // -> left (default) | center | right | "<number>" | "-<number>"

  /* Widget visibility! */
  alwaysShow: 0, // -> 0 (default) | 1 | 2

  /* Adaptive colors! */
  adaptiveColors: true, // -> true (default) | false
  minContrast: 2.6, // -> 2.6 (default) | number

  /* Dual-colored progress bar! */
  dualProgressBar: false, // -> true | false (default)

  /* Cache setting! */
  cacheMaxDays: 15, // -> 15 (default) | <number>

  /* Check for updates */
  checkForUpdates: false, // -> true (default) | false
};

/* ROOT STYLING */

export const className = `
  font-family: "JetBrainsMono Nerd Font",-apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, Helvetica, Arial, sans-serif, "Apple Color Emoji", "Segoe UI Emoji", "Segoe UI Symbol";
  color: #f8f8f2;

  * {
    box-sizing: border-box;
    padding: 0;
    border: 0;
    margin: 0;
  }
`;

/* EMOTION COMPONENTS */

const wrapperPos = ({ horizontal, vertical }) => {
  if (horizontal === "center" && vertical === "center") {
    return `
      top: 50%;
      left: 50%;
      transform: translate(-50%, -50%);
    `;
  }

  let hPos, vPos;
  switch (horizontal) {
    case "left":
      hPos = `left: 20px;`;
      break;
    case "center":
      hPos = `left: 50%; transform: translateX(-50%);`;
      break;
    case "right":
      hPos = `right: 20px;`;
      break;
    default:
      hPos = horizontal.startsWith("-")
        ? `right: ${horizontal.slice(1)};`
        : `left: ${horizontal};`;
      break;
  }
  switch (vertical) {
    case "top":
      vPos = `top: 20px;`;
      break;
    case "center":
      vPos = `top: 50%; transform: translateY(-50%);`;
      break;
    case "bottom":
      vPos = `bottom: 20px;`;
      break;
    default:
      vPos = vertical.startsWith("-")
        ? `bottom: ${vertical.slice(1)};`
        : `top: ${vertical};`;
      break;
  }

  return `${hPos} ${vPos}`;
};

const Wrapper = styled("div")`
  position: absolute;
  border-radius: 20px;
  overflow: hidden;
  opacity: ${(props) => (props.show ? 1 : 0)};
  background: ${(props) => (props.bg !== undefined ? props.bg : "#282a36")};
  transition: all 0.6s cubic-bezier(0.22, 1, 0.36, 1);
  ${wrapperPos}

  &::before {
    content: "";
    position: absolute;
    top: 0;
    left: 0;
    bottom: 0;
    right: 0;
    border-radius: 30px;
    z-index: -1;
  }

  * {
    transition: all 0.6s cubic-bezier(0.22, 1, 0.36, 1);
  }
`;

const miniWrapperPos = ({ horizontal }) => {
  switch (horizontal) {
    case "left":
      return `text-align: left;`;
    case "center":
      return `text-align: center;`;
    case "right":
      return `text-align: right;`;
    default:
      return horizontal.startsWith("-")
        ? `text-align: right;`
        : `text-align: left;`;
  }
};

const MiniWrapper = styled(Wrapper)`
  border-radius: 0;
  overflow: visible;
  box-shadow: none;
  background: transparent;
  ${miniWrapperPos}

  &::before {
    display: none;
  }
`;

const BigPlayer = styled("div")`
  display: flex;
  flex-direction: column;
  width: 240px;
`;

const MediumPlayer = styled(BigPlayer)`
  width: 180px;
`;

const SmallPlayer = styled("div")`
  position: relative;
  border: 1.5px solid #292828;
  border-radius: 20px;
  z-index: 30;
  display: flex;
  height: 30px;
  width: 300px;
`;

const MiniPlayer = styled("div")`
  position: relative;
  display: flex;
  flex-direction: column;
  width: 400px;
  line-height: 1;

  * {
    text-shadow: 0px 0px 4px #0004, 0px 2px 12px #0004;
  }

  > * + * {
    margin-top: 0.5em;
  }
`;

const ArtworkWrapper = styled("div")`
  position: relative;
  width: 240px;
  height: 240px;

  &.medium {
    width: 180px;
    height: 180px;
  }

  &.small {
    width: 25px;
    height: 25px;
  }

  &::before {
    position: absolute;
    content: "";
    left: 0;
    bottom: 0;
    right: 0;
    border-radius: 30px;
    background: #fff1;
    z-index: -1;
  }
`;

const Artwork = styled("img")`
  position: absolute;
  border-radius: 20px;
  top: 1.2px;
  left: 0.5px;
  bottom: 0;
  right: 0;
  height: 100%;
  object-fit: cover;
  opacity: ${(props) => (props.show ? 1 : 0)};
`;

const Information = styled("div")`
  position: relative;
  padding: 0.35em 0.75em;
  line-height: 1.3;
  border-radius: 30px;

  > p {
    text-align: center;
    white-space: nowrap;
    overflow: hidden;
    text-overflow: ellipsis;
  }

  &.small {
    flex: 1;
    width: 0;
    flex-direction: column;
    justify-content: center;
    border-radius: 80px;
    line-height: 1.3;
  }

  &.small > p {
    text-align: left;
  }
`;

const Progress = styled("div")`
  position: absolute;
  top: 0;
  left: 0;
  right: 0;
  height: 4px;
  background: ${(props) =>
    options.dualProgressBar && props.emptyColor
      ? props.emptyColor + "80"
      : "transparent"};

  &::after {
    content: "";
    position: absolute;
    top: 0;
    left: 0;
    bottom: 0;
    width: ${(props) => props.percent}%;
    background: ${(props) =>
      props.progressColor ? props.progressColor : "#f8f8f2"};
    transition: width 0.6s cubic-bezier(0.22, 1, 0.36, 1);
  }

  &.small {
    top: initial;
    bottom: 0;
  }

  &.mini {
    position: relative;
    height: 4px;
    border-radius: 80px;
    background: ${(props) =>
      options.dualProgressBar && props.emptyColor
        ? props.emptyColor + "60"
        : "#0005"};
    overflow: hidden;
  }
`;

const Track = styled("p")`
  font-weight: bold;
  font-size: 0.7em;
  color: ${(props) => (props.color ? props.color : "inherit")};

  &::after {
    content: "";
    display: inline-block;
    width: 0;
  }

  &.small {
    font-size: 0.65em;
  }

  &.mini {
    font-size: 1.2em;
  }
`;

const Artist = styled("p")`
  font-size: 0.5em;
  color: ${(props) => (props.color ? props.color : "inherit")};

  &::after {
    content: "";
    display: inline-block;
    width: 0;
  }

  &.small {
    font-size: 0.45em;
  }

  &.mini {
    font-size: 1em;
  }
`;

const Album = styled("p")`
  font-size: 0.65em;
  color: ${(props) => (props.color ? props.color : "inherit")};
  opacity: 0.75;

  &::after {
    content: "";
    display: inline-block;
    width: 0;
  }

  &.small {
    font-size: 0.65em;
  }
`;

const UpdateNotif = styled("div")`
  position: absolute;
  top: 0;
  width: 100%;
  padding: 1em;
  min-height: 30%;
  background-image: linear-gradient(to bottom, #000b, transparent);
`;

const UpdateText = styled("p")`
  font-size: 0.7em;
  text-align: center !important;

  a {
    color: inherit;
    text-decoration: none;

    &:hover {
      text-decoration: underline;
    }
  }
`;

/* UEBER-SPECIFIC STUFF */

export const init = (dispatch) => {
  // Initialize and clear cache of old artwork
  run(
    `mkdir -p UeberPlayer.widget/cache &&\
    find UeberPlayer.widget/cache -mindepth 1 -type f -mtime +${options.cacheMaxDays} -delete &&\
    osascript UeberPlayer.widget/lib/init.scpt`
  );
  // Check for updates when enabled
  if (options.checkForUpdates) {
    checkForUpdate(dispatch);
    setInterval(checkForUpdate, 86400000, dispatch);
  }
};

export const command = "osascript UeberPlayer.widget/lib/getTrack.scpt";

export const initialState = {
  app: "", // Current music software being used
  playing: false, // If currently playing a soundtrack
  songChange: false, // If the song changed
  primaryColor: undefined, // Primary color from artwork
  secondaryColor: undefined, // Secondary color from artwork
  tercaryColor: undefined, // Tercary color from artwork
  artwork: {
    // Artwork source URL to be used
    art1: "UeberPlayer.widget/default.png",
    art2: "UeberPlayer.widget/default.png",
    alternate: true,
  },
  song: {
    track: "", // Name of soundtrack
    artist: "", // Name of artist
    album: "", // Name of album
    localArtwork: "", // Locally stored url for album artwork
    onlineArtwork: "", // Online url for album artwork
    duration: 0, // Total duration of soundtrack in seconds
    elapsed: 0, // Total time elapsed in seconds
  },
};

// Update state
export const updateState = ({ type, output, error }, previousState) => {
  switch (type) {
    case "UB/COMMAND_RAN":
      return updateSongData(output, error, previousState);
    case "GET_ART":
      if (options.adaptiveColors) {
        return getColors(output, previousState, options);
      } else {
        const { art1, art2, alternate } = previousState.artwork;
        return {
          ...previousState,
          songChange: false,
          primaryColor: undefined,
          secondaryColor: undefined,
          tercaryColor: undefined,
          artwork: {
            art1: alternate ? art1 : output.img.src,
            art2: !alternate ? art2 : output.img.src,
            alternate: !alternate,
          },
        };
      }
    case "DEFAULT_ART":
      const { art1, art2, alternate } = previousState.artwork;
      return {
        ...previousState,
        songChange: false,
        primaryColor: undefined,
        secondaryColor: undefined,
        tercaryColor: undefined,
        artwork: {
          art1: alternate ? art1 : "UeberPlayer.widget/default.png",
          art2: !alternate ? art2 : "UeberPlayer.widget/default.png",
          alternate: !alternate,
        },
      };
    default:
      console.error("Invalid dispatch type?");
      return previousState;
  }
};

/* FUNCTIONS */

// Update song metadata
const updateSongData = (output, error, previousState) => {
  // Check for errors
  if (error) {
    const err_output = { ...previousState, error: error };
    console.error(err_output);
    return err_output;
  }

  // Extract & parse applescript output
  let [
    playing,
    app,
    track,
    artist,
    album,
    artworkURL,
    artworkFilename,
    duration,
    elapsed,
  ] = output.split(" @@ ");

  playing = playing === "true";
  duration = Math.floor(parseFloat(duration));
  elapsed = Math.floor(parseFloat(elapsed));

  // State controller
  if (!playing) {
    // If player is paused
    return { ...previousState, app, playing };
  } else if (
    track !== previousState.song.track ||
    album !== previousState.song.album
  ) {
    // Song change
    return {
      ...previousState,
      app,
      playing,
      songChange: true,
      song: {
        track,
        artist,
        album,
        localArtwork: `UeberPlayer.widget/cache/${artworkFilename}`,
        onlineArtwork: artworkURL,
        duration,
        elapsed,
      },
    };
  } else {
    // Currently playing
    return {
      ...previousState,
      app,
      playing,
      song: {
        ...previousState.song,
        elapsed,
      },
    };
  }
};

// Prepare artwork
const prepareArtwork = (dispatch, song) => {
  // Use a dummy image to test images beforehand
  const img = new Image();

  // Attempts images in this order: Local -> Online -> Default
  img.onload = () => {
    dispatch({ type: "GET_ART", output: { img } });
  };
  img.onerror = () => {
    if (
      song.onlineArtwork !== "missing value" &&
      img.src !== song.onlineArtwork
    ) {
      img.crossOrigin = "anonymous";
      img.src = song.onlineArtwork;
    } else {
      dispatch({ type: "DEFAULT_ART" });
    }
  };

  img.crossOrigin = "anonymous";
  img.src = song.localArtwork;
};

// RENDERING //
// Artwork image
const artworkImage = (wrapperClass, { art1, art2, alternate }) => (
  <ArtworkWrapper className={wrapperClass}>
    <Artwork src={art1} show={alternate} />
    <Artwork src={art2} show={!alternate} />
  </ArtworkWrapper>
);

// Small player component
const small = (
  { track, artist, elapsed, duration },
  secondaryColor,
  tercaryColor,
  artwork
) => (
  <SmallPlayer>
    {artworkImage("small", artwork)}
    <Information className="small">
      <Track color={secondaryColor}>
        {track} // {artist}
      </Track>
      <Artist color={tercaryColor}></Artist>
      <Progress
        progressColor={secondaryColor}
        emptyColor={tercaryColor}
        className="small"
        percent={(elapsed / duration) * 100}
      />
    </Information>
  </SmallPlayer>
);

// Render function
export const render = (
  {
    app,
    playing,
    songChange,
    primaryColor,
    secondaryColor,
    tercaryColor,
    artwork,
    song,
  },
  dispatch
) => {
  const { size, horizontalPosition, verticalPosition, alwaysShow } = options;

  // Determine widget visability
  const showWidget = playing || (alwaysShow === 1 && app) || alwaysShow === 2;

  // When song changes, prepare artwork
  if (songChange) {
    prepareArtwork(dispatch, song);
  }

  // Render
  return (
    <Wrapper
      show={showWidget}
      bg={primaryColor}
      horizontal={horizontalPosition}
      vertical={verticalPosition}
    >
      {small(song, secondaryColor, tercaryColor, artwork)}
    </Wrapper>
  );
};
