package tool.file;

import java.io.File;
import java.io.FileInputStream;
import java.security.MessageDigest;

/**
 * MD5チェックサム値を扱うためのユーティリティクラスです
 */
public class MD5Sum {
	
	private static final String MD5_ALGORITHM = "MD5";
	
	/**
	 * ファイルのmd5チェックサム値をString型で返します
	 * 
	 * @param inputFile チェックサム値を求めるファイル情報
	 * @return MD5チェックサム値
	 * @throws Exception アルゴリズムが存在しない場合、もしくはファイル入力エラー
	 */
	public static String getMD5Sum(File inputFile) throws Exception{

		MessageDigest md = null;

		// MD5のMessageDigestインスタンスを取得
		md = MessageDigest.getInstance(MD5_ALGORITHM);
			
		// ファイル読込開始
		FileInputStream in = new FileInputStream(inputFile);

		byte[] dat = new byte[256];
		int len;
		while ((len = in.read(dat)) >=0) {
			md.update(dat, 0, len);//dat配列の先頭からlenまでのダイジェストを計算する
		}
		    
		// ファイルクローズ
		in.close();
		
		// 文字列に変換して返却
	    return MD5Byte2Str(md.digest());
	}
	
	// バイト型配列に設定されたMD5値を文字列に変換
	private static String MD5Byte2Str(byte[] md5sum){

		String retStr = "";
		
		// バイト値ごとに変換
		for (int i = 0; i < md5sum.length; i++) {

			int d = md5sum[i];

			// 負数を変換
			if (d < 0) {
				d += 256;
			}
			// 上位1けたに0を追加する
			if (d < 16) {
				retStr += "0";
			}
			// byte値を16進数の文字列に変換
			retStr += Integer.toString(d, 16);
		}
		
		return retStr;
	}
}
