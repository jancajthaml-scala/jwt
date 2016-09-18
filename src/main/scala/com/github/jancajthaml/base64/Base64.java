package com.github.jancajthaml.base64;

public class Base64 {

    private static final char[] toBase64URL = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_".toCharArray();
    private static final int[] fromBase64URL = new int[256];
    static {
        java.util.Arrays.fill(fromBase64URL, -1);
        int iS = 0;
        int i = 0;
        for (i = 0, iS = toBase64URL.length; i < iS; i++) {
            fromBase64URL[toBase64URL[i]] = i;
        }
        fromBase64URL['='] = 0;
    }

    private Base64() {}

    public static Encoder getEncoder() { return Encoder.instance; }

    public static class Encoder {

        private Encoder() {}

        private static final Encoder instance = new Encoder();

        public String encode(byte[] src) {
            int sLen = src != null ? src.length : 0;
            if (sLen == 0) {
                return "";
            } else {
                int eLen = sLen / 3 * 3;
                int cCnt = ((sLen - 1) / 3 + 1) << 2;
                int dLen = cCnt;
                byte[] dest = new byte[dLen];

                for (int s = 0, d = 0; s < eLen;) {
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
                    dest[dLen - 2] = left == 2 ? (byte) toBase64URL[i & 0x3f] : (byte) '=';
                    dest[dLen - 1] = '=';
                }
                return new String(dest, 0, 0, dest.length).replaceAll("=", "");
            }
        }

    }

    public static Decoder getDecoder() { return Decoder.instance; }

    public static class Decoder {

        private Decoder() {}

        private static final Decoder instance = new Decoder();

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