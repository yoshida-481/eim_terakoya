package jp.co.ctc_g.eim.app.document.business.service;

import java.awt.image.BufferedImage;
import java.io.File;
import java.util.List;

import org.apache.pdfbox.pdmodel.PDDocument;
import org.apache.pdfbox.rendering.ImageType;

/**
 * PDFの作成を行うサービスクラスです。
 */
public interface PDFService {

	/**
	 * PDFファイルに対し、画像を埋め込みます。<br>
	 * 
	 * @param pdfFile 表紙PDF
	 * @param image 画像
	 * @param imageScale 画像サイズ
	 * @param imagePosX 画像出力開始位置(X軸)
	 * @param imagePosY 画像出力開始位置(Y軸)
	 * @throws Exception 例外処理
	 * @since Ver 6.6 
	 */
	public void putImage(File pdfFile, BufferedImage image, int imageScale, int imagePosX, int imagePosY) throws Exception;
	
	/**
	 * PDFファイル(PDDocument形式)の指定されたページを画像化して返します。<br>
	 * 
	 * @param doc PDFドキュメント
	 * @param pageIndex ページインデックス(最初のページは0とする)
	 * @param imageType 画像のタイプ
	 * @return 画像化されたPDFファイル
	 * @throws Exception 例外処理
	 * @since Ver 6.6 
	 */
	public BufferedImage toImage(PDDocument doc, int pageIndex, ImageType imageType) throws Exception;
	
	/**
	 * PDFファイルを任意のページ毎に分割し、新しいPDFファイルを作成します。<br>
	 * 作成したPDFはフルパスをリストとして返します。
	 * @param doc PDFドキュメント
	 * @param path 保存先のパス
	 * @param startPage 分割開始ページ(1ページから。1,2,3,...)
	 * @param endPage 分割終了ページ
	 * @return 切り出したPDFファイル
	 * @throws Exception 例外処理
	 * @since Ver 6.6 
	 */
	public File split(PDDocument doc, String path, int startPage, int endPage) throws Exception;
	
	/**
	 * 表紙用PDFファイルを作成します。
	 * @param fontFile フォントファイルのフルパス
	 * @param filePath 作成PDFフルパス
	 * @param fontSize フォントサイズ
	 * @param body 表示する文字情報リスト
	 * @param cdatePosX 作成日出力開始X座標
	 * @param cdatePosY 作成日出力開始Y座標
	 * @param attrPosX 属性出力開始X座標
	 * @param attrPosY 属性出力開始Y座標
	 * @param attrLineHeightLarge タイトル属性間行間隔
	 * @param attrLineHeight 属性出力行間隔
	 * @param langId 表示言語
	 * @throws Exception 例外処理
	 * @since Ver 6.6 
	 */
	public void make(
			String fontFile
			, String filePath
			, List<String> body
			, int fontSize
			, float cdatePosX
			, float cdatePosY
			, float attrPosX
			, float attrPosY
			, int attrLineHeightLarge
			, int attrLineHeight
			, String langId
		) throws Exception;
}
