package common.util;
import java.text.DecimalFormat;
/**
 * フォーマットするユーティリティクラス
 *
 */
public class FormatUtil {

	/**
	 * doubleを数字だけの表現でフォーマットした文字列を返す。
	 * だだし、有効桁数は15桁なので、それ以上の桁数は保証しない。
	 * @param value フォーマットするdoubleの値
	 * @return
	 */
	public static String getDoubleFormatedString(Double value){
		DecimalFormat df=new DecimalFormat();
		df.applyPattern(AppConstant.DOUBLE_MAX_STRING_FORMAT);
		return df.format(value);
	}
}
