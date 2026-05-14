# Windhawk Mod Settings

## Windows 11 Taskbar Styler (V1.6.1)

```
theme: RosePine
controlStyles:
  - target: Taskbar.TaskbarFrame > Rectangle
    styles:
      - Fill=Transparent
  - target: Grid#SystemTrayFrameGrid
    styles:
      - Background=Transparent
  - target: Grid#SystemTrayFrameGrid
    styles:
      - Background=Transparent
  - target: Taskbar.TaskListLabeledButtonPanel@CommonStates > Rectangle#RunningIndicator
    styles:
      - Height=18
      - Width=20
      - RadiusX=4
      - RadiusY=4
      - StrokeThickness=2
      - Stroke@ActiveNormal=#ebbcba
      - Stroke@ActivePointerOver=#ebbcba
      - Stroke@ActivePressed=#ebbcba
      - Fill=Transparent
      - VerticalAlignment=1
styleConstants:
  - ''
```

---

## Windows 11 Start Menu Styler(V1.5.1)
```
theme: RosePine
disableNewStartMenuLayout: ''
styleConstants:
  - ''
controlStyles:
  - target: ''
    styles:
      - ''
themeResourceVariables:
  - ''
webContentStyles:
  - target: ''
    styles:
      - ''
webContentCustomJs: ''
```

---

## Taskbar Height and Icon Size (V1.3.7)

```
IconSize: 12
TaskbarButtonWidth: 30
TaskbarHeight: 28
IconSizeSmall: 16
TaskbarButtonWidthSmall: 28
```

---

## Taskbar Clock Customization (V1.7.4)

```
ShowSeconds: 0
TimeFormat: hh':'mm tt
DateFormat: ddd',' MMM dd yyyy
WeekdayFormat: dddd
WeekdayFormatCustom: Sun, Mon, Tue, Wed, Thu, Fri, Sat
TopLine: ''
BottomLine: '%date%  %time%  '
MiddleLine: '%date%  %time%  '
TooltipLine: ''
TooltipLineMode: append
Width: 180
Height: 50
MaxWidth: 0
TextSpacing: 0
DataCollection:
  NetworkMetricsFormat: mbs
  NetworkMetricsFixedDecimals: -1
  PercentageFormat: spacePaddingAndSymbol
  UpdateInterval: 1
  NetworkAdapterName: ''
  GpuAdapterName: ''
MediaPlayer:
  IgnoredPlayers:
    - ''
  MaxLength: 28
  NoMediaText: No media
  RemoveBrackets: 0
WebContentWeatherLocation: ''
WebContentWeatherFormat: '%c 🌡️%t 🌬️%w'
WebContentWeatherUnits: autoDetect
WebContentsItems:
  - Url: https://rss.nytimes.com/services/xml/rss/nyt/World.xml
    BlockStart: <item>
    Start: <title>
    End: </title>
    ContentMode: ''
    SearchReplace:
      - Search: ''
        Replace: ''
    MaxLength: 28
WebContentsUpdateInterval: 10
TimeZones:
  - Eastern Standard Time
TimeStyle:
  Hidden: 1
  TextColor: ''
  TextAlignment: ''
  FontSize: 0
  FontFamily: ''
  FontWeight: ''
  FontStyle: ''
  FontStretch: ''
  CharacterSpacing: 0
  # Visible: 0
DateStyle:
  Hidden: 0
  TextColor: ''
  TextAlignment: ''
  FontSize: 14
  FontFamily: Cascadia Mono
  FontWeight: Light
  FontStyle: ''
  FontStretch: ''
  CharacterSpacing: 0
oldTaskbarOnWin11: 0
```

---

## Taskbar Labels for Windows 11 (V1.4.2)

```
mode: noLabelsWithoutCombining
taskbarItemWidth: 160
runningIndicatorStyle: centerFixed
progressIndicatorStyle: sameAsRunningIndicatorStyle
excludedPrograms:
  - excluded1.exe
minimumTaskbarItemWidth: 50
maximumTaskbarItemWidth: 176
fontSize: 12
fontFamily: ''
textTrimming: characterEllipsis
leftAndRightPaddingSize: 8
spaceBetweenIconAndLabel: 8
runningIndicatorHeight: 0
runningIndicatorVerticalOffset: 0
alwaysShowThumbnailLabels: 0
labelForSingleItem: '%name%'
labelForMultipleItems: '[%amount%] %name%'
```

---

## Middle click to close on the taskbar (V1.0.9)

```
multipleItemsBehavior: closeAll
keysToEndTask:
  Ctrl: 1
  Alt: 0
oldTaskbarOnWin11: 0
```


