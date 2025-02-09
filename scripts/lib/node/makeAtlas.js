#!/usr/bin/env node

import fg from "fast-glob"
import fs from "fs"
import fsp from "fs/promises"
import path from "path"
import sharp from "sharp";
import { exec } from "child_process";
import { makePNG, transform } from "./BMPLib.js"
import { GrowingPacker } from "./imagePacker.js"
import cac from "cac"
import dedent from "./dedent.js"

const __filename = import.meta.url.replace("file://","")
const __dirname = path.dirname(__filename)
const __cwd = process.cwd()

// Argument Parser
const cli = cac("makeAtlas").version("1.0.0")

cli.command("[...files]", "Generate a Texture Atlas from a list of files. Glob notation accepted.")
    .option("-o, --output [out]", "Choose an output directory for the built Texture Atlas.", { default: "" })
    .option("-f, --offset-format [format]", "Choose the format of the offset list (haskell, json)", { default: "json" })
    .option("-s, --sorter [sorter]", "Choose the sorter for the sprite order on the atlas (none, width, height, maxside, area, random)", { default: "height" })
    .option("-e, --extentions [out]", "Choose the image types accepted (bmp,png,jpg) as a list. (E.g: [bmp,png])", { default: "[bmp,png]" })
    .option("-d, --directory-sort", "If enabled, each subdirectory will generate it's own atlas (only top-level subdirectories)", { default: false })
    .option("--notrim", "If enabled, transparent space around the images will not be removed.", { default: false })

cli.help()

const args = cli.parse()

// If user requests help, display help message (pre-generated) and exit the program.
if (args.options.help) process.exit(0)

// Crash in event of unknown options.
const unknownOpts = Object.keys(args.options).filter(k => 
    ![
        "--", "o", "f", "s", "e", "d", 
        "output", "offsetFormat", "sorter", "extentions", "directorySort", "notrim"
    ].includes(k)
)
if (unknownOpts.length > 0) throw new Error(`Unknown option(s): ${unknownOpts.join(", ")}`)

// Settings
const tempPath = path.join(__dirname, "temp")
const tempPngPath = path.join(tempPath, "png")
const tempBmpPath = path.join(tempPath, "bmp")

if (args.options.extentions && !args.options.extentions.match(/^\[(bmp|png|jpg|jpeg)+(,(bmp|png|jpg|jpeg))*\]$/)) 
    throw new Error("Invalid extention list: Not a valid list.")

const extentions = [
    args.options.extentions.includes("bmp") ? "bmp" : "",
    args.options.extentions.includes("png") ? "png" : "",
    args.options.extentions.includes("jpg") ? "jpg" : "",
    args.options.extentions.includes("jpeg") ? "jpeg" : ""
]

// Cleanup in case of crash on last execution
await fsp.rm(tempPath, { recursive: true, force: true })

// Ensure temp directories
await fsp.mkdir(tempPngPath, { recursive: true })
await fsp.mkdir(tempBmpPath, { recursive: true })

const outputPath = path.isAbsolute(args.options.output) 
    ? args.options.output 
    : path.join(__cwd, args.options.output)

if (path.basename(outputPath).includes(".") && !fs.statSync(outputPath).isDirectory()) throw new Error("Invalid output path: Not a directory.")

// Input images
const imagePathList = args.args
if (!imagePathList || imagePathList.length == 0) throw new Error("No input files provided.")

const imagePaths = imagePathList.map(p => path.join(__cwd, p))
for (let ip of imagePaths) {
    if (ip.includes("*")) continue;

    if (!fs.existsSync(ip)) throw new Error("Could not find file " + ip)
    if (!fs.statSync(ip).isFile()) throw new Error("Is bot a file: " + ip)
    if (!extentions.includes(path.extname(ip).replace(".",""))) throw new Error("Not a valid image: " + ip) //  !== "bmp"
}

// Recursively read input files
const imageList = await fg(imagePaths, { absolute: true, markDirectories: true, globstar: true });

// Make Image Table
const imageTableRoot = []

// Read image files
for (let filePath of imageList) {
    if (filePath.includes(outputPath)) continue;

    const bmpFile = await fsp.readFile(filePath)
    const { image: _imageData, data: rawData } = await transform(bmpFile, "#fc0fc0", true)
    const imageData = args.options.notrim ? _imageData : _imageData.trim()
    const imageMetadata = await imageData.metadata()

    imageTableRoot.push({
        type: path.extname(filePath).replace(".",""),
        id: path.basename(filePath).split(".").slice(0, -1).join("."),
        dir: path.basename(path.dirname(filePath.replace(__cwd, ""))),
        image: imageData,
        raw: rawData,
        w: imageMetadata.width,
        h: imageMetadata.height
    })
}

// Partition image list into their respective partitions, if the directorySort flag is present.
const imageTable = 
    args.options.directorySort
        ? imageTableRoot.reduce((acc, cur) => {
            const dir = cur.dir === "" ? "atlas" : cur.dir

            if (!(dir in acc)) {
                acc[dir] = []
            }

            acc[dir].push(cur)

            return acc
        }, {})
        : { atlas: imageTableRoot }

const offsets = {}
for (const sector in imageTable) {
    const table = imageTable[sector].slice(0)
    // Order sprites in atlas
    const packer = new GrowingPacker()

    const sorter = args.options.sorter
    if (!["none", "width", "height", "maxside", "area", "random"].includes(sorter)) throw new Error(`Unknown sorter: '${sorter}'`)

    GrowingPacker.sorters.now(sorter,table)
    packer.fit(table)
    let { w,h } = packer.root

    // Create empty atlas
    const atlas = sharp({
        create: {
            width: w,
            height: h,
            channels: 4,
            background: { r: 255, g: 255, b: 255, alpha: 0 }
        }
    })

    // Preprare sprites for composition
    const compositeImageTable = []
    for (const i of table) {
        if (!i.fit) throw new Error(`Could not make atlas: unable to fit '${i.id}' using current settings.`)

        const image = i.image
        const bitmapData = await image.raw().toBuffer({ resolveWithObject: true })
        const compositionData = { 
            input: bitmapData.data,
            raw: {
                width: bitmapData.info.width,
                height: bitmapData.info.height,
                channels: bitmapData.info.channels
            },
            gravity: "northwest", 
            left: i.fit.x, 
            top: i.fit.y 
        }

        compositeImageTable.push(compositionData)
    }

    // Compose sprites into atlas
    atlas.composite(compositeImageTable)

    // Generate sprite offset list
    offsets[sector] = table.reduce((acc, cur) => cur.fit ? {...acc, [cur.id]: { x: cur.fit.x, y: cur.fit.y, w: cur.w, h: cur.h }} : acc, {})

    // Save atlas to temporary PNG
    const atlasPNGPath = path.join(tempPngPath, `${sector}.png`)
    await makePNG(atlas.png(), "#fc0fc0", atlasPNGPath)
}

async function asyncExec(cmd, options, callback) {
    return new Promise((resolve, reject) => {
        exec(cmd, options, (err, stderr, stdout) => {
            callback(err, stderr, stdout)
            resolve()
        })
    })
}

// Convert atlas to BMP
await asyncExec(`mogrify -format bmp -path ${tempBmpPath} *.png`, { cwd: tempPngPath }, (error, stdout, stderr) => {
    if (error) {
        throw new Error("Could not launch bitmap converter:", error)
        return;
    }

    if (stderr) {
        throw new Error("Could not convert bitmap:", stderr)
        return;
    }
});

// Ensure output directory
await fsp.mkdir(outputPath, { recursive: true })

// Place atlas and offset file into output directory
const atlasFiles = await fg(path.join(tempBmpPath, `*.bmp`), { absolute: true, markDirectories: true })
for (const atlas of atlasFiles) {
    const atlasName = path.basename(atlas, path.extname(atlas))
    await fsp.copyFile(path.join(tempBmpPath, `${atlasName}.bmp`), path.join(outputPath, `${atlasName}.bmp`))
}


if (args.options.offsetFormat === "json") {
    await fsp.writeFile(path.join(outputPath, "offsets.json"), JSON.stringify(offsets, null, 4))
} else if (args.options.offsetFormat === "haskell") {
    
    const data = dedent`
        import Data.Map (Map)
        import qualified Data.Map as Map

        data OffsetEntry = OffsetEntry {
            x :: Int,
            y :: Int,
            w :: Int,
            h :: Int
        }

        type OffsetAtlas = Map String OffsetEntry

        data Offsets = Offsets {
            ${Object.keys(offsets).map(k => `${k} :: OffsetAtlas`).join(",\n    ")}
        }

        offsets = Offsets {
            ${
                Object.entries(offsets).map(([key,value]) => 
                    `${key} = Map.fromList [${
                        Object.entries(value).map(([k2,v2]) => `("${k2}", OffsetEntry ${v2.x} ${v2.y} ${v2.w} ${v2.h})`).join(",")
                    }]`
                ).join(",\n    ")
            }
        }
`

    await fsp.writeFile(path.join(outputPath, "offsets.hs"), data)
} else throw new Error(`Could not generate offset list: Unknown format: '${args.options.offsetFormat}'`)


// Cleanup
await fsp.rm(tempPath, { recursive: true, force: true })


console.log(`Texture atlas generated at ${outputPath}`)
process.exit(0)