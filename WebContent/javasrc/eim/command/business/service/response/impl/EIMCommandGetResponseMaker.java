package eim.command.business.service.response.impl;

import java.io.File;

import eim.command.business.service.result.EIMCommandResult;
import eim.command.business.service.result.EIMCommandResultData;
import eim.command.common.util.EIMCommandUtil;

/**
 * get/getById/checkinByUserコマンド用レスポンス返却実装クラス
 * 
 *
 */
public class EIMCommandGetResponseMaker extends EIMCommandResponseMakerBinary {
    
	@Override
	protected String getHeaderFileName(EIMCommandResult resultData) throws Exception
	{
		return EIMCommandUtil.encode(((EIMCommandResultData)resultData).getFileName());
	}

	@Override
	protected File getFileObj(EIMCommandResult resultData) throws Exception
	{
		return ((EIMCommandResultData)resultData).getFile();
	}

}
