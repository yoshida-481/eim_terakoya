package jp.co.ctc_g.eim.app.document.business.service;

import java.awt.image.BufferedImage;

/**
 * QRコードの生成と読取を行うサービスクラスです。
 */
public interface QRCodeService {

	/**
	 * QRコードの生成
	 * @param source QRコードに変換する文字列
	 * @param size QRコード画像のサイズ(一辺のドット数)
	 * @return 生成されたQRコードの画像データ
	 * @throws Exception QRコード作成失敗
	 * @since Ver 6.6 
	 */
	public BufferedImage create(String source, int size) throws Exception;
	
	/**
	 * QRコードの読取
	 * @param image 読み取るQRコードの画像データ
	 * @return 読み取った文字情報。QRコードを認識できなかった場合、null。
	 * @since Ver 6.6 
	 */
	public String read(BufferedImage image);
}
