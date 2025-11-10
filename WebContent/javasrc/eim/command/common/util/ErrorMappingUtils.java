package eim.command.common.util;

import eim.bo.EIMException;
import eim.bo.EIMResource;
import eim.util.*;

/**
 * 
 * 内部IF/外部IFのエラーケースマッピング用Util
 *
 */
public class ErrorMappingUtils {

	/**
	 * 指定の内部IFエラーに相当する外部IFのエラーコードを取得する
	 * <pre>
	 * <設定ファイルで指定されたプレフィクス>.<内部IFエラーメッセージキー>をキーとするエラーコードをリソースファイルから取得
	 * </pre>
	 * @param internalError
	 * @return 内部IFエラーが空 あるいは 設定された内部IFのメッセージキーがない場合はnullを返却
	 * @throws EIMException 
	 */
	public static String getExternalErrorCode(EIMException internalError) throws EIMException {
		
		String errorCode = null;
		
		if (internalError != null) {
			String messageKey = internalError.getMessageKey();
			String prefix = EIMConfig.getValue("EIM.COMMAND.IN.EX.ERROR.MAPPING.PREFIX");
			if (!StringUtils.isBlank(messageKey) && !StringUtils.isBlank(prefix)) {
				errorCode = EIMResource.getMessageValue(prefix + "." + messageKey);
			}
		}
		
		return errorCode;
	}
}
