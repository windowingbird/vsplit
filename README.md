# vsplit
利用ffmpeg对视频进行分割和切片。

分割命令：b站网页版只支持最大8G文件上传，投稿助手只支持最大4G文件上传，本命令默认用帧复制(copy)的方式将flv格式的bilibili录播视频平均分割为约时长1小时20分钟的flv文件(约4GiB大小)。

切片命令：将`--ss`和`--to`参数传入ffmpeg，利用cpu对需要切片的视频片段进行libx265编码，对音频部分进行aac重编码，导出mp4格式的视频。

声音切片命令：只对视频中的音频进行切片。

合并命令：用帧复制(copy)的方式合并多个视频。

## 用法/Usage

### 支持的命令/commands
``` bash
$ vsplit -h
Usage: vsplit COMMAND

Available options:
  -h,--help                Show this help text

Available commands:
  split                    split a video file
  cut                      cut a video file
  listen                   listen a video file
  combine                  Combine video files
  version                  Print version
```

### 分割命令/split command
``` bash
$ vsplit -h split
Usage: vsplit split [-d|--duration DURATION] [-n|--number NUMBER] 
                    [-D|--dst DIST] SOURCE
  split a video file

Available options:
  -d,--duration DURATION   specify split duration, default value is 4800
  -n,--number NUMBER       specify output file base number, default value is 0
  -D,--dst DIST            specify dist directory, default value is ./out
  SOURCE                   specify source file
  -h,--help                Show this help text
```

### 切片命令/cut command
``` bash
$ vsplit -h cut
Usage: vsplit cut [--ss START] [--to END] [--vb VIDEO BITRATE] [-o|--out OUT]
                  SOURCE
  cut a video file

Available options:
  --ss START               specify start time, format HH:MM:SS[.SSS], optional
  --to END                 specify end time, format HH:MM:SS[.SSS], optional
  --vb VIDEO BITRATE       specify video bitrate, default 4M, optional
  --ab AUDIO BITRATE       specify audio bitrate, default 128K, optional
  -o,--out OUT             specify output file, default value is output.mp4
  SOURCE                   specify source file
  -h,--help                Show this help text
```

### 声音切片命令/listen command
``` bash
$ vsplit -h listen
Usage: vsplit listen [--ss START] [--to END] [-f|--format AUDIO FORMAT] 
                     [--ab AUDIO BITRATE] [-v|--volume VOLUME] [-o|--out OUT]
                     SOURCE
  listen a video file

Available options:
  --ss START               specify start time, format HH:MM:SS[.SSS], optional
  --to END                 specify end time, format HH:MM:SS[.SSS], optional
  -f,--format AUDIO FORMAT specify audio format, default aac, optional
  --ab AUDIO BITRATE       specify audio bitrate, default 128K, optional
  -v,--volume VOLUME       specify volume adjustment, format [-]XdB, optional
  -o,--out OUT             specify output file, default value is output.m4a,
                           file extension for mp3 and aac format will be
                           automaticlly alternatived
  SOURCE                   specify source file
  -h,--help                Show this help text
```

### 合并命令/combine command
``` bash
$ vsplit combine -h
Usage: vsplit combine [INPUTS] [-o|--out OUT]
  Combine video files

Available options:
  INPUTS                   specify input files
  -o,--out OUT             specify output file, default value is p1.flv
  -h,--help                Show this help text
```

## 安装/install
1. 下载这个仓库/download this repo
2. 执行仓库根目录，执行`stack install`/enter the root directory of the repo, excute `stack install`
