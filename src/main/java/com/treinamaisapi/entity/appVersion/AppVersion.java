package com.treinamaisapi.entity.appVersion;

import jakarta.persistence.*;
import lombok.Getter;
import lombok.Setter;

import java.time.LocalDateTime;

@Getter
@Setter
@Entity
@Table(name = "app_version")
public class AppVersion {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(name = "version_name", nullable = false)
    private String versionName;

    @Column(name = "version_code", nullable = false)
    private Integer versionCode;

    @Column(name = "apk_url", nullable = false)
    private String apkUrl;

    @Column(name = "description")
    private String description;

    @Column(name = "force_update")
    private Boolean forceUpdate;

    @Column(name = "created_at")
    private LocalDateTime createdAt = LocalDateTime.now();

}
