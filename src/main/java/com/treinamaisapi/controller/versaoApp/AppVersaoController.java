package com.treinamaisapi.controller.versaoApp;

import com.treinamaisapi.common.dto.versaoApp.AppVersionDTO;
import com.treinamaisapi.common.dto.versaoApp.CreateAppVersionDTO;
import com.treinamaisapi.controller.swagger.AppVersaoControllerSwagger;
import com.treinamaisapi.service.AppVersion.AppVersionService;
import lombok.RequiredArgsConstructor;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.*;

@RequiredArgsConstructor
@RestController
@RequestMapping(path = "/api/app", produces = MediaType.APPLICATION_JSON_VALUE)
public class AppVersaoController implements AppVersaoControllerSwagger {

    private final AppVersionService service;

    @Override
    @GetMapping("/version")
    public AppVersionDTO getVersion() {
        return service.getLatestVersion();
    }

    @PostMapping("/create/version")
    @Override
    public void createVersion(@RequestBody CreateAppVersionDTO dto) {
        service.createVersion(dto);
    }

}
