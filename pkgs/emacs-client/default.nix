{ runCommand, emacs, writeText }:

let
  script = ./emacs-client.scpt;
  plist = writeText "Info.plist" ''
    <?xml version="1.0" encoding="UTF-8"?>
    <!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
    <plist version="1.0">
    <dict>
    	<key>CFBundleAllowMixedLocalizations</key>
    	<true/>
    	<key>CFBundleDevelopmentRegion</key>
    	<string>en</string>
    	<key>CFBundleDocumentTypes</key>
    	<array>
    		<dict>
    			<key>CFBundleTypeExtensions</key>
    			<array>
    				<string>*</string>
    			</array>
    			<key>CFBundleTypeIconFile</key>
    			<string>document.icns</string>
    			<key>CFBundleTypeName</key>
    			<string>All</string>
    			<key>CFBundleTypeOSTypes</key>
    			<array>
    				<string>****</string>
    			</array>
    			<key>CFBundleTypeRole</key>
    			<string>Viewer</string>
    		</dict>
    	</array>
    	<key>CFBundleExecutable</key>
    	<string>droplet</string>
    	<key>CFBundleIconFile</key>
    	<string>Emacs.icns</string>
    	<key>CFBundleIdentifier</key>
    	<string>com.apple.ScriptEditor.id.EmacsClient</string>
    	<key>CFBundleInfoDictionaryVersion</key>
    	<string>6.0</string>
    	<key>CFBundleName</key>
    	<string>EmacsClient</string>
    	<key>CFBundlePackageType</key>
    	<string>APPL</string>
    	<key>CFBundleShortVersionString</key>
    	<string>1.0</string>
    	<key>CFBundleSignature</key>
    	<string>dplt</string>
    	<key>CFBundleURLTypes</key>
    	<array>
    		<dict>
    			<key>CFBundleURLName</key>
    			<string>org-protocol</string>
    			<key>CFBundleURLSchemes</key>
    			<array>
    				<string>org-protocol</string>
    			</array>
    		</dict>
    	</array>
    	<key>LSMinimumSystemVersionByArchitecture</key>
    	<dict>
    		<key>x86_64</key>
    		<string>10.6</string>
    	</dict>
    	<key>LSRequiresCarbon</key>
    	<true/>
    	<key>NSAppleEventsUsageDescription</key>
    	<string>This script needs to control other applications to run.</string>
    	<key>NSAppleMusicUsageDescription</key>
    	<string>This script needs access to your music to run.</string>
    	<key>NSCalendarsUsageDescription</key>
    	<string>This script needs access to your calendars to run.</string>
    	<key>NSCameraUsageDescription</key>
    	<string>This script needs access to your camera to run.</string>
    	<key>NSContactsUsageDescription</key>
    	<string>This script needs access to your contacts to run.</string>
    	<key>NSHomeKitUsageDescription</key>
    	<string>This script needs access to your HomeKit Home to run.</string>
    	<key>NSMicrophoneUsageDescription</key>
    	<string>This script needs access to your microphone to run.</string>
    	<key>NSPhotoLibraryUsageDescription</key>
    	<string>This script needs access to your photos to run.</string>
    	<key>NSRemindersUsageDescription</key>
    	<string>This script needs access to your reminders to run.</string>
    	<key>NSSiriUsageDescription</key>
    	<string>This script needs access to Siri to run.</string>
    	<key>NSSystemAdministrationUsageDescription</key>
    	<string>This script needs access to administer this system to run.</string>
    	<key>OSAAppletShowStartupScreen</key>
    	<false/>
    </dict>
    </plist>
  '';
in runCommand "emacs-client" { __useChroot = true; } ''
  mkdir -p $out/Applications
  /usr/bin/osacompile -o $out/Applications/EmacsClient.app ${script}
  cp ${emacs}/Applications/Emacs.app/Contents/Resources/{document,Emacs}.icns $out/Applications/EmacsClient.app/Contents/Resources/
  cp -f ${plist} $out/Applications/EmacsClient.app/Contents/Info.plist
''
