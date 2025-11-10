package addon.util;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics2D;
import java.awt.RenderingHints;
import java.awt.geom.AffineTransform;
import java.awt.geom.Ellipse2D;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import java.text.SimpleDateFormat;
import java.util.Date;

import javax.imageio.ImageIO;

import eim.net.EIMSession;

/**
 * 印鑑のイメージを作成する機能を提供するクラス。
 */
public class CreateStampImage {
	/** 画像フォーマット。*/
    private static final String IMG_FORMAT = "png";
    /** 円の線の幅。*/
    private static final int IMG_LINE_WIDTH = 2;
    /** イメージのタイプ。 {@link java.awt.image.BufferedImage#TYPE_3BYTE_BGR}
     * または、{@link java.awt.image.BufferedImage#TYPE_INT_ARGB}を指定します。 */
    private static final int IMG_TYPE = BufferedImage.TYPE_3BYTE_BGR;
    /** イメージの背景色。
     * イメージのタイプが{@link java.awt.image.BufferedImage#TYPE_INT_ARGB}
     * 以外のときに有効。*/
    private static final Color IMG_BGCOLOR = Color.white;

    /**
     * 印鑑の画像を作成してファイルに保存します。
     * <ul>
     * <li>作成される印鑑画像の説明<br/>
     *      imageSizeで指定した直径の円を描き、その中にuserNameで指定した
     *      文字列を１文字ずつ縦に並べて表示した印鑑の画像を作成します。
     *      文字はfontNameで指定したフォントを使用し、円の内部に納まるように
     *      位置と大きさを自動調整します。画像の背景色は、
     *      {@link java.awt.Color#white}を使用します。
     * </li>
     * <li>画像を保存するファイルの説明</br>
     *      画像を保存先するファイルは、fileOutputPathで指定されたディレクトリに
     *      保存されます。またファイルの名前は、
     *      「<i>&lt;yyyyMMddHHmmssSSS形式のサーバー時刻&gt;</i>_<i>&lt;セッションID&gt;</i>.png」
     *      となります。
     * </li>
     * </ul>
     * 
     * @param sess {@link jakarta.servlet.http.HttpServlet}を指定します。
     * @param userName 印鑑のイメージに使用する文字列を指定します。
     * @param fontName 印鑑のイメージに使用するフォント名を指定します。
     * @param imageSize イメージのサイズ(円の直径)をピクセルで指定します。
     * @param fileOutputPath ファイルの出力パスを指定します。
     * 
     * @return 作成された印鑑のイメージを保存したファイルを返します。
     * @throws Exception イメージファイルの作成に失敗した場合。エラーの原因は
     *      以下のとおりです。
     *      <ul>
     *      <li>画像を保存するファイルが作成できない
     *          ({@link java.io.IOException}が発生した場合)</li>
     *      </ul>
     */
    public File create(String userName, String fontName, int imageSize, String fileOutputPath, String datetime)
                throws Exception {
        try {
            String imageFileName = datetime + "." + IMG_FORMAT;
            File imageFile = new File(fileOutputPath, imageFileName);
            BufferedImage readImage = null;

            readImage = new BufferedImage(imageSize + IMG_LINE_WIDTH * 2,
                        imageSize + IMG_LINE_WIDTH * 2, IMG_TYPE);

            Graphics2D off = readImage.createGraphics();

            // Imageのタイプが透過イメージでないときは、背景色で塗りつぶす。
            if (IMG_TYPE != BufferedImage.TYPE_INT_ARGB) {
                off.setBackground(IMG_BGCOLOR);
                off.clearRect(0, 0, readImage.getWidth(), readImage.getHeight());
            }

            off.setRenderingHint(RenderingHints.KEY_ANTIALIASING, 
                    RenderingHints.VALUE_ANTIALIAS_ON);

            BasicStroke wideStroke = new BasicStroke(IMG_LINE_WIDTH);
            off.setStroke(wideStroke);

            // 円の描画
            off.setPaint(Color.red);
            off.draw(new Ellipse2D.Double(IMG_LINE_WIDTH, IMG_LINE_WIDTH, imageSize, imageSize));

            // FONTの設定
            AffineTransform fontAT = new AffineTransform(); 
            //Font theFont = new Font(fontName, Font.PLAIN, IMG_FONT_SIZE); 
            Font theFont = new Font(fontName, Font.PLAIN, imageSize); 
            // userNameを縦書きにしたときの幅と高さを算出する。
            double[] layoutBound = getTextLayoutRectangle(userName, off, theFont);
            // 縦書きのuserNameの幅、高さが同じになるようにscaleを調整
            double xscale = imageSize / Math.sqrt(2) / layoutBound[0];
            double yscale = imageSize / Math.sqrt(2) / layoutBound[1];
            fontAT.setToScale(xscale, yscale);   // 描画後の幅と高さを揃える。
            Font theDerivedFont = theFont.deriveFont(fontAT); 
            off.setFont(theDerivedFont);

            // テキストの描画
            FontMetrics metrics = off.getFontMetrics();
            float x = 0f;
            float y = (float)(IMG_LINE_WIDTH + imageSize * (1 - 1 / Math.sqrt(2)) / 2
                    + fontHeight(off, theDerivedFont));
            for (int idx = 0; idx < userName.length(); idx++) {
                x = IMG_LINE_WIDTH + (imageSize - metrics.charWidth(userName.charAt(idx))) / 2;
                String str = userName.substring(idx, idx + 1);
                off.drawString(str, x, y);
                y += fontHeight(off, off.getFont());
            }

            boolean result = false;
            // 作成したイメージをファイルに保存
            result = ImageIO.write(readImage, IMG_FORMAT, imageFile);
            if (! result) {
                throw new Exception(
                        new IllegalArgumentException("specified image writer not supported"));
            }
            return imageFile;
        } catch (IOException e) {
            throw new Exception(e);
        } catch (Exception e) {
            throw new Exception(e);
        }
    }

    /**
     * 指定された文字を１文字ずつ縦に並べたときの幅と高さを取得します。
     * @param input 文字列を指定します。
     * @param graphics 文字を出力する{@link java.awt.Graphics2D}オブジェクトを
     *      指定します。
     * @param font 文字のフォントを指定します。
     * @return 配列の最初の要素に最大の文字幅、２番目の要素に縦の長さを格納した
     *      配列オブジェクトを返します。
     */
    private double[] getTextLayoutRectangle(String input, Graphics2D graphics, Font font) {
        FontMetrics metrics = graphics.getFontMetrics(font);
        double[] bound = new double[2];

        for (int idx = 0; input != null && idx < input.length(); idx++) {
            // 文字幅の最大値をbound[0]に格納する。
            if (metrics.charWidth(input.charAt(idx)) > bound[0]) {
                bound[0] = metrics.charWidth(input.charAt(idx));
            }
            // 文字の高さをインクリメントする。
            bound[1] += fontHeight(graphics, font);
        }
        return bound;
    }

    /**
     * フォントの高さを取得します。
     * @param graphics 文字を出力する{@link java.awt.Graphics2D}オブジェクトを
     *      指定します。
     * @param font 文字のフォントを指定します。
     * @return 指定されたフォントの文字の高さを返します。
     */
    private int fontHeight(Graphics2D graphics, Font font) {
        AffineTransform trans = font.getTransform();
        graphics.getFontMetrics(font);
        int charHeight = 0;
        charHeight =  (int)(font.getSize() * trans.getScaleY());
        return charHeight;
    }
	
}
