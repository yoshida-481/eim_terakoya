package jp.co.ctc_g.eim.app.document.business.util;

import java.util.ArrayList;
import java.util.List;

import jp.co.ctc_g.eim.framework.business.domain.entity.UserDomain;
import jp.co.ctc_g.eim.framework.common.util.EIMThreadContext;

public class AppMailUtils {

	/**
	 * 入力されたユーザリストから、重複を排除します。
	 * また、セッションユーザも排除するように指定できます。
	 * @param userList 処理対象
	 * @param removeSessionUser trueならセッションユーザも排除する
	 * @return 処理結果
	 */
	public static List<UserDomain> removeDuplicateUser(List<UserDomain> userList, boolean removeSessionUser) throws Exception
	{
		//探索中、一度処理したユーザIDのリスト
		List<Long> existIdList = new ArrayList<Long>();
		
		long sessionUserId = EIMThreadContext.getEIMSession().getUser().getId();
		
		//入力されたリストを末尾から探索し、２度目以降の同一ID要素が現れたらリストから排除する。
		for (int i = userList.size()-1; 0<=i; i--)
		{
			Long userId = (long)userList.get(i).getId();
			
			if (removeSessionUser && sessionUserId == userId) {
				userList.remove(i);
			}
			else if ( existIdList.contains(userId) ) {
				userList.remove(i);
			}
			else {
				existIdList.add(userId);
			}
		}
		
		return userList;
	}
	
}
