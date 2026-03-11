package com.treinamaisapi.controller.versaoApp;

import com.treinamaisapi.common.dto.versaoApp.AppVersionDTO;
import com.treinamaisapi.controller.swagger.AppVersaoControllerSwagger;
import lombok.RequiredArgsConstructor;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RequiredArgsConstructor
@RestController
@RequestMapping(path = "/api/app", produces = MediaType.APPLICATION_JSON_VALUE)
public class AppVersaoController implements AppVersaoControllerSwagger {

    @GetMapping("/version")
    @Override
    public AppVersionDTO getVersion() {

        AppVersionDTO dto = new AppVersionDTO();

        dto.setVersion("1.0.0");
        dto.setApkUrl("https://treinamais.com/app/treinamais.apk");
        dto.setForceUpdate(false);

        return dto;
    }

}
