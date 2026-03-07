# Windhawk Mod Settings

## Windows 11 Taskbar Styler (v1.5.2)

```
theme: RosePine
controlStyles:
  - target: Rectangle
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
resourceVariables:
  - variableKey: ''
    value: ''
```


---

## Taskbar Clock Customization (v1.7.1)

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
  Visible: 0
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

## Taskbar Height and Icon Size (v1.3.6)

```
TaskbarHeight: 26
IconSize: 12
TaskbarButtonWidth: 24
IconSizeSmall: 14
TaskbarButtonWidthSmall: 24
```


