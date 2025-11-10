package jp.co.ctc_g.eim.app.document.common.util;

import java.util.ArrayList;
import java.util.List;

/**
 * 【ドキュメントAPI】
 */
public class StringUtils {

	/**
	 * 拡張子がついたファイル名から拡張子を取得します。
	 *
	 * @param fileName ファイル名
	 * @return 拡張子
	 * @throws Exception
	 */
	public static String getFileExt(String fileName) throws Exception {
		int pos = fileName.lastIndexOf(".");
		if (pos == -1) {
			return null;
		}

		String result = fileName.substring(pos);
		return result;
	}

	/**
	 * 入力文字列がnullの場合にはブランクに置換します。
	 *
	 * @param str 入力文字列
	 * @return 置換した文字列
	 */
	public static String nullToBlank(String str) {

		return (str != null ? str : "");
	}

	/**
	 * 文字列チェック。
	 *
	 * @param str
	 * @return
	 */
	public static boolean isEmpty(String str) {

		return eim.util.StringUtils.isBlank(str);
	}

	/**
	 * 拡張子がついたファイル名から拡張子なしのファイル名を取得します。
	 *
	 * @param fileName ファイル名
	 * @return ファイル名（拡張子なし）
	 * @throws Exception
	 */
	public static String getFileBody(String fileName) throws Exception {
		int pos = fileName.lastIndexOf(".");
		if (pos == -1) {
			return fileName;
		}

		String result = fileName.substring(0, pos);
		return result;
	}

	/**
	 * 引数で取得したリストに含まれる値の前後に*を入力して返します。
	 *
	 * @param valueList リスト
	 * @return 前後に*を付与した文字列リスト
	 * @throws Exception
	 */
	public static List<String> insertAsterrisk(List<String> valueList) throws Exception {
		List<String> result = new ArrayList<String>();
		for(int i=0; i<valueList.size(); i++){
			if(valueList.get(i) != null && valueList.get(i).length() > 0){
				result.add("*" + valueList.get(i) +"*");
			}
		}

		return result;
	}

	/**
	 * 引数文字列の正規表現のメタ文字をエスケープします。
	 *
	 * @param source 引数文字列
	 * @return 正規表現のメタ文字をエスケープした文字列
	 * @throws Exception
	 */
	public static String escapeRegex(String source) {

		String target = new String(source);

		target = target.replaceAll("\\\\", "\\\\\\\\");
		target = target.replaceAll("\\*", "\\\\*");
		target = target.replaceAll("\\+", "\\\\+");
		target = target.replaceAll("\\.", "\\\\.");
		target = target.replaceAll("\\?", "\\\\?");
		target = target.replaceAll("\\{", "\\\\{");
		target = target.replaceAll("\\}", "\\\\}");
		target = target.replaceAll("\\(", "\\\\(");
		target = target.replaceAll("\\)", "\\\\)");
		target = target.replaceAll("\\[", "\\\\[");
		target = target.replaceAll("\\]", "\\\\]");
		target = target.replaceAll("\\^", "\\\\^");
		target = target.replaceAll("\\$", "\\\\\\$");
		target = target.replaceAll("\\-", "\\\\-");
		target = target.replaceAll("\\|", "\\\\|");
		target = target.replaceAll("\\/", "\\\\/");

		return target;
	}

}
