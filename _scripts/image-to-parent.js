const glob = require("glob");
const fs = require("fs");
const path = require("path");

const entry = path.join(__dirname, "../");

const list = glob
  .sync(`${entry}/**/**.md`)
  .filter($ => !$.includes("/node_modules"));

const images = glob
  .sync(`${entry}/**/**.{jpg,png,jpeg,gif,bmp}`)
  .filter($ => !$.includes("node_modules"));

const imageCt = [];
const imageMd = path.join(__dirname, "../_book/images.md");
const imageAssets = path.join(__dirname, "../_book/assets");

if (!fs.existsSync(imageAssets)) {
  fs.mkdirSync(imageAssets);
}

function run() {
  images.forEach((image, i) => {
    const imgCurrent = image.split("/source_md")[1];
    const imgName = image.split("/").pop();
    const imgPath = path.join(imageAssets, imgName);
    if (!fs.existsSync(image)) {
      return;
    }
    fs.createReadStream(image).pipe(fs.createWriteStream(imgPath));
    imageCt.push(`
    ### 图片 ${i}
    \`${imgCurrent}\`
    ![${imgName}](./assets/${imgName})
  
  
  
    `);
  });
  console.log("rt");
  fs.writeFileSync(imageMd, imageCt.join("\n"));
  return;
  const fileMap = {};

  const ct = [];
  for (file of list) {
    const p = file.split("/source_md")[1];
    const k = p.split("/");
    const data = {
      file,
      firstPath: "./" + p.split("/")[1]
      // lastPath: './' + k.join('/'),
    };
    // console.log(data)
    // 主路径是根路径处理
    if (data.firstPath.endsWith(".md")) {
      data.firstPath = "./";
    }
    data.assetsPath = `${data.firstPath}/assets`;

    fileMap[file] = data;

    if (file.includes("awesome")) {
      break;
    }
    // 开始处理图片
    handle(data);
  }
  function handle(data = {}) {
    const { file, assetsPath } = data;
    let content = fs.readFileSync(file).toString();
    // 找到所有的图片
    const images = content.match(/\!\[.+\]\(.+\)/gim);
    if (!images) {
      return;
    }
    images.forEach(image => {
      const imageRe = image.match(/\((.+)\)/);
      const imagePath = imageRe[1];
      const imageName = imagePath.split("/").pop();
      // 当前路径
      let imageRealPath = "";
      // 网络图片
      if (imagePath.startsWith("http")) {
        console.log(imagePath);
        imageRealPath = imagePath;
      } else {
        imageRealPath = path.join(data.firstPath, imagePath);
      }
      const imageTargetPath = path.join(data.assetsPath, imageName);

      const tmpDir = path.join(__dirname, "../_book/assets");
      // 图片统一位置
      const tmp = path.join(tmpDir, imageName);
      if (!fs.existsSync(tmpDir)) {
        fs.mkdirSync(tmpDir);
      }
      fs.createReadStream(imageRealPath).pipe(fs.createWriteStream(tmp));
      ct.push("### 图片" + ct.length);
      ct.push(`\n\npath: \`${imageRealPath}\`\n`);
      const tName = imagePath.startsWith("http")
        ? imagePath
        : `./assets/${imageName}`;
      ct.push(`![${imageName}](${tName})\n\n`);

      if (imageTargetPath !== imageRealPath) {
        console.log("不在指定位置:", imageRealPath, imageTargetPath);
        const t = path.join(entry, imageTargetPath);
        const r = path.join(entry, imageRealPath);

        if (!fs.existsSync(data.assetsPath)) {
          fs.mkdirSync(data.assetsPath);
        }
        if (!fs.existsSync(r)) {
          console.log("源文件不存在");
          return;
        }

        // fs.createReadStream(r).pipe(fs.createWriteStream(t))
        // fs.unlinkSync(r)
        // console.log('已移动并删除')
      }
    });
  }
}

function _imgtsource() {
  const _source_images = images.filter($ => $.includes("_book/_img"));
  const sourceImagesMap = {};
  _source_images.forEach(image => {
    const soureImageName = image.split("/").pop();
    sourceImagesMap[soureImageName] = image;
  });
  console.log(_source_images);
  const _target_images = images.filter($ => !$.includes("_book/_img"));
  _target_images.forEach(image => {
    const targetImageName = image.split("/").pop();
    // 存在修改后的
    if (sourceImagesMap[targetImageName]) {
      fs.createReadStream(sourceImagesMap[targetImageName]).pipe(
        fs.createWriteStream(image)
      );
      console.log(`${targetImageName} rewrite`);
    }
  });
}

_imgtsource();
