import path from "path"
import fsp from "fs/promises"
import Jimp from "jimp"
import * as bmp from "@vingle/bmp-js";
import sharp from "sharp";
import replaceColor from "replace-color"

const BUF_BMP = Buffer.from([0x42, 0x4d]); // "BM" file signature

function isBitmap(buf) {
  return Buffer.compare(BUF_BMP, buf.slice(0, 2)) === 0;
}

export async function transform(input, bgColor, raw = false) {
    /** @type {{ sharpInstance: sharp.Sharp, rawData: { data: Buffer; raw: { width: number;  height: number; channels: number; } }  }} */
    const { sharpInstance, rawData } = ((buf) => {
        if (isBitmap(buf)) {
            const bitmap = bmp.decode(buf, true);

            const rawData = {
                data: bitmap.data,
                raw: {
                    width: bitmap.width,
                    height: bitmap.height,
                    channels: 4,
                }
            }

            return {
                sharpInstance: sharp(bitmap.data, {
                    raw: {
                        width: bitmap.width,
                        height: bitmap.height,
                        channels: 4,
                    },
                }),
                rawData
            }
        }
        return { sharpInstance: sharp(buf), rawData: {} };
    })(input);

    const img = sharpInstance//.flatten({ background: bgColor }) // "#fc0fc0"

    if (raw) return { image: img.png({ progressive: true }), data: rawData }
    else return img.png({ progressive: true })//.toBuffer();
}

export async function makePNG(input, bgColor, filePath) {
    const jpegbuf = await input.toBuffer()
    /** @type {Awaited<ReturnType<typeof Jimp.read>>} */
    const jimg_replaced = await replaceColor({
        image: jpegbuf,
        colors: {
            type: "hex",
            targetColor: bgColor,//"#fc0fc0"
            replaceColor: "#00000000"
        },
        deltaE: 10
    })

    await jimg_replaced.writeAsync(filePath)
}