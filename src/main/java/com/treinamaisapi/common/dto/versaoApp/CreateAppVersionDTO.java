package com.treinamaisapi.common.dto.versaoApp;

import lombok.Data;

@Data
public class CreateAppVersionDTO {

    private String versionName;
    private Integer versionCode;
    private String apkUrl;
    private String description;
    private Boolean forceUpdate;

}
