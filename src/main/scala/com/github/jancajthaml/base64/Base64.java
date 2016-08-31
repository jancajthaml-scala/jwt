package com.github.jancajthaml.base64;

public class Base64 {

    private static final char[] toBase64URL = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_".toCharArray();
    private static final int[] fromBase64URL = { -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 62, -1, -1, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, -1, -1, -1, 0, -1, -1, -1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, -1, -1, -1, -1, 63, -1, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1 };

    private Base64() {}

    public static Encoder getEncoder() { return Encoder.instance; }

    public static class Encoder {

        private Encoder() {}

        static final Encoder instance = new Encoder();

        public String encode(byte[] src) {
            int sLen = src != null ? src.length : 0;
            if (sLen == 0) {
                return "";
            } else {
                int eLen = (sLen / 3) * 3;
                int cCnt = ((sLen - 1) / 3 + 1) << 2;
                int dLen = cCnt;
                byte[] dest = new byte[dLen];

                for (int s = 0, d = 0, cc = 0; s < eLen;) {
                    int i = (src[s++] & 0xff) << 16 | (src[s++] & 0xff) << 8 | (src[s++] & 0xff);

                    dest[d++] = (byte) toBase64URL[(i >>> 18) & 0x3f];
                    dest[d++] = (byte) toBase64URL[(i >>> 12) & 0x3f];
                    dest[d++] = (byte) toBase64URL[(i >>> 6) & 0x3f];
                    dest[d++] = (byte) toBase64URL[i & 0x3f];
                }

                int left = sLen - eLen;
                if (left > 0) {
                    int i = ((src[eLen] & 0xff) << 10) | (left == 2 ? ((src[sLen - 1] & 0xff) << 2) : 0);
                    dest[dLen - 4] = (byte) toBase64URL[i >> 12];
                    dest[dLen - 3] = (byte) toBase64URL[(i >>> 6) & 0x3f];
                    if (left == 2) {
                        dest[dLen - 2] = (byte) toBase64URL[i & 0x3f];
                    }
                }
                return new String(dest, 0, 0, dest.length);
            }
        }

    }

    public static Decoder getDecoder() { return Decoder.instance; }

    public static class Decoder {

        private Decoder() {}

        static final Decoder instance = new Decoder();

        public String decode(byte[] src) {
            int sLen = src.length;
            if (sLen == 0) {
                return "";
            }

            int sIx = 0, eIx = sLen - 1;
            while (sIx < eIx && fromBase64URL[src[sIx] & 0xff] < 0) {
                sIx++;
            }
            while (eIx > 0 && fromBase64URL[src[eIx] & 0xff] < 0) {
                eIx--;
            }

            int pad = src[eIx] == '=' ? (src[eIx - 1] == '=' ? 2 : 1) : 0;
            int cCnt = eIx - sIx + 1;
            int sepCnt = sLen > 76 ? (src[76] == '\r' ? cCnt / 78 : 0) << 1 : 0;
            int len = ((cCnt - sepCnt) * 6 >> 3) - pad;
            byte[] dest = new byte[len];
            int d = 0;
            for (int cc = 0, eLen = (len / 3) * 3; d < eLen;) {
                int i = fromBase64URL[src[sIx++]] << 18 | fromBase64URL[src[sIx++]] << 12 | fromBase64URL[src[sIx++]] << 6 | fromBase64URL[src[sIx++]];
                dest[d++] = (byte) (i >> 16);
                dest[d++] = (byte) (i >> 8);
                dest[d++] = (byte) i;
                if (sepCnt > 0 && ++cc == 19) {
                    sIx += 2;
                    cc = 0;
                }
            }

            if (d < len) {
                int i = 0;
                for (int j = 0; sIx <= eIx - pad; j++) {
                    i |= fromBase64URL[src[sIx++]] << (18 - j * 6);
                }

                for (int r = 16; d < len; r -= 8) {
                    dest[d++] = (byte) (i >> r);
                }
            }

            return new String(dest, 0, 0, dest.length);
        }
    }

}