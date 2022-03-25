package scala.meta.internal.builds

import java.nio.file.Files
import java.nio.file.Path

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

import scala.meta.internal.builds.WorkspaceLoadedStatus.Installed
import scala.meta.internal.metals.MetalsEnrichments._
import scala.meta.internal.metals.clients.language.MetalsLanguageClient
import scala.meta.io.AbsolutePath

/**
 * Helper trait for build tools that have a Bloop plugin
 */
trait BloopInstallProvider { this: BuildTool =>

  /**
   * Args necessary for build tool to generate the .bloop files.
   */
  def bloopInstallArgs(workspace: AbsolutePath): List[String]

  /**
   * Method used to generate the necesary .bloop files for the
   * build tool.
   */
  def bloopInstall(
      workspace: AbsolutePath,
      systemProcess: List[String] => Future[WorkspaceLoadedStatus]
  )(implicit ec: ExecutionContext): Future[WorkspaceLoadedStatus] = {
    this.digest(workspace) match {
      case None => doBloopInstall(workspace, systemProcess)
      case Some(currentDigest) => {
        val bloopDir = workspace.resolve(".bloop").toNIO

        createCacheIfPossible(bloopDir)

        if (cacheExists(bloopDir, currentDigest)) {
          restoreCache(bloopDir, currentDigest)
          writeDigestRecord(bloopDir, currentDigest)
          Future.successful(WorkspaceLoadedStatus.Installed)
        } else {
          scribe.info("No bloop config cache found - running bloopInstall")
          doBloopInstall(workspace, systemProcess).map { status =>
            status match {
              case Installed => {
                writeDigestRecord(bloopDir, currentDigest)
              }
              case _ => ()
            }
            status
          }
        }
      }
    }
  }

  private def bloopCacheDir(
      bloopDir: Path,
      digest: String
  ): Path = {
    bloopDir.resolve(s"bloop-cache-$digest")
  }

  private def buildDigestRecordFile(
      bloopDir: Path
  ): Path = {
    bloopDir.resolve("build-digest")
  }

  private def writeDigestRecord(bloopDir: Path, digest: String): Unit = {
    Files.writeString(buildDigestRecordFile(bloopDir), digest)
  }

  private def readDigestRecord(bloopDir: Path): Option[String] = {
    val digestRecordFile = buildDigestRecordFile(bloopDir)
    if (Files.exists(digestRecordFile)) {
      Some(Files.readString(digestRecordFile))
    } else {
      None
    }
  }

  private def createCacheIfPossible(bloopDir: Path): Unit = {
    readDigestRecord(bloopDir).foreach { digest =>
      val cacheDir = bloopCacheDir(bloopDir, digest)

      if (!Files.exists(cacheDir)) {
        scribe.info(s"Creating cache for digest $digest")
        Files.createDirectory(cacheDir)
        Files
          .list(bloopDir)
          .asScala
          .filter(file => file.getFileName.toString.endsWith(".json"))
          .foreach { path =>
            Files.move(path, cacheDir.resolve(path.filename))
          }
      }
    }
  }

  private def restoreCache(bloopDir: Path, digest: String): Unit = {
    val cacheDir = bloopCacheDir(bloopDir, digest)
    scribe.info(s"Restoring bloop config cache for digest $digest")
    Files
      .list(cacheDir)
      .asScala
      .foreach { path =>
        Files.copy(path, bloopDir.resolve(path.getFileName()))
      }
  }

  private def cacheExists(bloopDir: Path, digest: String): Boolean = {
    val cacheDir = bloopCacheDir(bloopDir, digest)
    Files.exists(cacheDir)
  }

  def doBloopInstall(
      workspace: AbsolutePath,
      systemProcess: List[String] => Future[WorkspaceLoadedStatus]
  ): Future[WorkspaceLoadedStatus] = {
    systemProcess(bloopInstallArgs(workspace))
  }
}
